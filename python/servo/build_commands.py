# Copyright 2013 The Servo Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

import datetime
import os
from os import PathLike
import os.path as path
import pathlib
import shutil
import stat
import subprocess
import sys

from time import time
from typing import Optional, List, Dict, Union

from mach.decorators import (
    CommandArgument,
    CommandProvider,
    Command,
)
from mach.registrar import Registrar

import servo.platform
import servo.platform.macos
import servo.util
import servo.visual_studio

from servo.command_base import BuildType, CommandBase, check_call
from servo.gstreamer import windows_dlls, windows_plugins, package_gstreamer_dylibs
from servo.platform.build_target import BuildTarget

from python.servo.platform.build_target import SanitizerKind

SUPPORTED_ASAN_TARGETS = [
    "aarch64-apple-darwin",
    "aarch64-unknown-linux-gnu",
    "aarch64-unknown-linux-ohos",
    "x86_64-apple-darwin",
    "x86_64-unknown-linux-gnu",
]

SUPPORTED_TSAN_TARGETS = [
    "aarch64-apple-darwin",
    "aarch64-unknown-linux-gnu",
    "x86_64-apple-darwin",
    "x86_64-unknown-linux-gnu",
]


def get_rustc_llvm_version() -> Optional[List[int]]:
    """Determine the LLVM version of `rustc` and return it as a List[major, minor, patch, ...]

    In some cases we want to ensure that the LLVM version of rustc and clang match, e.g.
    when using ASAN for both C/C++ and Rust code, we want to use the same ASAN implementation.
    This function assumes that rustc points to the rust compiler we are interested in, which should
    be valid in both rustup managed environment and on nix.
    """
    try:
        result = subprocess.run(["rustc", "--version", "--verbose"], encoding="utf-8", capture_output=True)
        result.check_returncode()
        for line in result.stdout.splitlines():
            line_lowercase = line.lower()
            if line_lowercase.startswith("llvm version:"):
                llvm_version = line_lowercase.strip("llvm version:")
                llvm_version = llvm_version.strip()
                version = llvm_version.split(".")
                print(f"Info: rustc is using LLVM version {'.'.join(version)}")
                return list(map(int, version))
        else:
            print(f"Error: Couldn't find LLVM version in output of `rustc --version --verbose`: `{result.stdout}`")
    except Exception as e:
        print(f"Error: Failed to determine rustc version: {e}")
    return None


@CommandProvider
class MachCommands(CommandBase):
    @Command("build", description="Build Servo", category="build")
    @CommandArgument("--jobs", "-j", default=None, help="Number of jobs to run in parallel")
    @CommandArgument(
        "--no-package", action="store_true", help="For Android, disable packaging into a .apk after building"
    )
    @CommandArgument("--verbose", "-v", action="store_true", help="Print verbose output")
    @CommandArgument("--very-verbose", "-vv", action="store_true", help="Print very verbose output")
    @CommandArgument("params", nargs="...", help="Command-line arguments to be passed through to Cargo")
    @CommandBase.common_command_arguments(build_configuration=True, build_type=True, package_configuration=True)
    def build(
        self,
        build_type: BuildType,
        jobs=None,
        params=None,
        no_package=False,
        verbose=False,
        very_verbose=False,
        sanitizer: SanitizerKind = SanitizerKind.NONE,
        flavor=None,
        **kwargs,
    ) -> int:
        opts = params or []

        if build_type.is_release():
            opts += ["--release"]
        elif build_type.is_dev():
            pass  # there is no argument for debug
        else:
            opts += ["--profile", build_type.profile]

        if jobs is not None:
            opts += ["-j", jobs]
        if verbose:
            opts += ["-v"]
        if very_verbose:
            opts += ["-vv"]
        self.config["build"]["sanitizer"] = sanitizer

        env = self.build_env()
        self.ensure_bootstrapped()
        self.ensure_clobbered()

        host = servo.platform.host_triple()
        target_triple = self.target.triple()

        if sanitizer.is_some():
            self.build_sanitizer_env(env, opts, kwargs, target_triple, sanitizer)

        build_start = time()

        if host != target_triple and "windows" in target_triple:
            if os.environ.get("VisualStudioVersion") or os.environ.get("VCINSTALLDIR"):
                print(
                    "Can't cross-compile for Windows inside of a Visual Studio shell.\n"
                    "Please run `python mach build [arguments]` to bypass automatic "
                    "Visual Studio shell, and make sure the VisualStudioVersion and "
                    "VCINSTALLDIR environment variables are not set."
                )
                sys.exit(1)

        # Gather Cargo build timings (https://doc.rust-lang.org/cargo/reference/timings.html).
        opts = ["--timings"] + opts

        crown_enabled = "enabled" if kwargs.get("use_crown", False) else "disabled (no JS garbage collection linting)"
        print(f"Building `{build_type.directory_name()}` build with crown {crown_enabled}.")
        if very_verbose:
            for key in env:
                print((key, env[key]))

        status = self.run_cargo_build_like_command("rustc", opts, env=env, verbose=verbose, **kwargs)

        if status == 0:
            built_binary = self.get_binary_path(build_type, sanitizer=sanitizer)

            if not no_package and self.target.needs_packaging():
                rv = Registrar.dispatch(
                    "package", context=self.context, build_type=build_type, flavor=flavor, sanitizer=sanitizer
                )
                if rv:
                    return rv

            if "windows" in target_triple:
                if not copy_windows_dlls_to_build_directory(built_binary, self.target):
                    status = 1

            elif "darwin" in target_triple:
                servo_bin_dir = os.path.dirname(built_binary)
                assert os.path.exists(servo_bin_dir)

                if self.enable_media:
                    library_target_directory = path.join(path.dirname(built_binary), "lib/")
                    if not package_gstreamer_dylibs(built_binary, library_target_directory, self.target):
                        return 1

                # On the Mac, set a lovely icon. This makes it easier to pick out the Servo binary in tools
                # like Instruments.app.
                try:
                    import Cocoa  # pyrefly: ignore[import-error]

                    icon_path = path.join(self.get_top_dir(), "resources", "servo_1024.png")
                    icon = Cocoa.NSImage.alloc().initWithContentsOfFile_(icon_path)
                    if icon is not None:
                        Cocoa.NSWorkspace.sharedWorkspace().setIcon_forFile_options_(icon, built_binary, 0)
                except ImportError:
                    pass

        # Print how long the build took
        elapsed = time() - build_start
        elapsed_delta = datetime.timedelta(seconds=int(elapsed))
        build_message = f"{'Succeeded' if status == 0 else 'Failed'} in {elapsed_delta}"
        print(build_message)
        assert isinstance(status, int)
        return status

    @Command("clean", description="Clean the target/ and Python virtual environment directories", category="build")
    @CommandArgument("--manifest-path", default=None, help="Path to the manifest to the package to clean")
    @CommandArgument("--verbose", "-v", action="store_true", help="Print verbose output")
    @CommandArgument("params", nargs="...", help="Command-line arguments to be passed through to Cargo")
    def clean(self, manifest_path=None, params=[], verbose=False) -> None:
        self.ensure_bootstrapped()

        virtualenv_path = path.join(self.get_top_dir(), ".venv")
        if path.exists(virtualenv_path):
            print("Removing virtualenv directory: %s" % virtualenv_path)
            shutil.rmtree(virtualenv_path)

        opts = ["--manifest-path", manifest_path or path.join(self.context.topdir, "Cargo.toml")]
        if verbose:
            opts += ["-v"]
        opts += params
        return check_call(["cargo", "clean"] + opts, env=self.build_env(), verbose=verbose)

    def build_sanitizer_env(
        self, env: Dict, opts: List[str], kwargs, target_triple: str, sanitizer: SanitizerKind = SanitizerKind.NONE
    ) -> None:
        if sanitizer.is_none():
            return
        # do not use crown (clashes with different rust version)
        env["RUSTC"] = "rustc"
        # Enable usage of unstable rust flags
        env["RUSTC_BOOTSTRAP"] = "1"
        # std library should also be instrumented
        opts += ["-Zbuild-std"]
        # We need to always set the target triple, even when building for host.
        kwargs["target_override"] = target_triple
        # When sanitizers are used we also want framepointers to help with backtraces.
        if "force-frame-pointers" not in env["RUSTFLAGS"]:
            env["RUSTFLAGS"] += " -C force-frame-pointers=yes"

        # Note: We want to use the same clang/LLVM version as rustc.
        rustc_llvm_version = get_rustc_llvm_version()
        if rustc_llvm_version is None:
            raise RuntimeError("Unable to determine necessary clang version for Sanitizer support")
        llvm_major: int = rustc_llvm_version[0]
        target_clang = f"clang-{llvm_major}"
        target_cxx = f"clang++-{llvm_major}"
        if shutil.which(target_clang) is None or shutil.which(target_cxx) is None:
            env.setdefault("TARGET_CC", "clang")
            env.setdefault("TARGET_CXX", "clang++")
        else:
            # libasan can be compatible across multiple compiler versions and has a
            # runtime check, which would fail if we used incompatible compilers, so
            # we can try and fallback to the default clang.
            env.setdefault("TARGET_CC", target_clang)
            env.setdefault("TARGET_CXX", target_cxx)
        # By default, build mozjs from source to enable Sanitizers in mozjs.
        env.setdefault("MOZJS_FROM_SOURCE", "1")

        # We need to use `TARGET_CFLAGS`, since we don't want to compile host dependencies with ASAN,
        # since that causes issues when building build-scripts / proc macros.
        # The actual flags will be appended below depending on the sanitizer kind.
        env.setdefault("TARGET_CFLAGS", "")
        env.setdefault("TARGET_CXXFLAGS", "")
        env.setdefault("RUSTFLAGS", "")

        if sanitizer.is_asan():
            if target_triple not in SUPPORTED_ASAN_TARGETS:
                print(
                    "AddressSanitizer is currently not supported on this platform\n",
                    "See https://doc.rust-lang.org/beta/unstable-book/compiler-flags/sanitizer.html",
                )
                sys.exit(1)

            # Enable asan
            env["RUSTFLAGS"] += " -Zsanitizer=address"
            env["TARGET_CFLAGS"] += " -fsanitize=address"
            env["TARGET_CXXFLAGS"] += " -fsanitize=address"

            # asan replaces system allocator with asan allocator
            # we need to make sure that we do not replace it with jemalloc
            self.features.append("servo_allocator/use-system-allocator")
        elif sanitizer.is_tsan():
            if target_triple not in SUPPORTED_TSAN_TARGETS:
                print(
                    "ThreadSanitizer is currently not supported on this platform\n",
                    "See https://doc.rust-lang.org/beta/unstable-book/compiler-flags/sanitizer.html",
                )
                sys.exit(1)
            env["RUSTFLAGS"] += " -Zsanitizer=thread"
            env["TARGET_CFLAGS"] += " -fsanitize=thread"
            env["TARGET_CXXFLAGS"] += " -fsanitize=thread"


def copy_windows_dlls_to_build_directory(servo_binary: str, target: BuildTarget) -> bool:
    servo_exe_dir = os.path.dirname(servo_binary)
    assert os.path.exists(servo_exe_dir)

    build_path = path.join(servo_exe_dir, "build")
    assert os.path.exists(build_path)

    # Copy in the built EGL and GLES libraries from where they were built to
    # the final build dirctory
    def find_and_copy_built_dll(dll_name: str) -> None:
        try:
            file_to_copy = next(pathlib.Path(build_path).rglob(dll_name))
            shutil.copy(file_to_copy, servo_exe_dir)
        except StopIteration:
            print(f"WARNING: could not find {dll_name}")

    print(" • Copying ANGLE DLLs to binary directory...")
    find_and_copy_built_dll("libEGL.dll")
    find_and_copy_built_dll("libGLESv2.dll")

    print(" • Copying GStreamer DLLs to binary directory...")
    if not package_gstreamer_dlls(servo_exe_dir, target):
        return False

    print(" • Copying MSVC DLLs to binary directory...")
    if not package_msvc_dlls(servo_exe_dir, target):
        return False

    return True


def package_gstreamer_dlls(servo_exe_dir: str, target: BuildTarget) -> bool:
    gst_root = servo.platform.get().gstreamer_root(target)
    if not gst_root:
        print("Could not find GStreamer installation directory.")
        return False

    missing = []
    for gst_lib in windows_dlls():
        try:
            shutil.copy(path.join(gst_root, "bin", gst_lib), servo_exe_dir)
        except Exception:
            missing += [str(gst_lib)]

    for gst_lib in missing:
        print("ERROR: could not find required GStreamer DLL: " + gst_lib)
    if missing:
        return False

    # Only copy a subset of the available plugins.
    gst_dlls = windows_plugins()

    gst_plugin_path_root = os.environ.get("GSTREAMER_PACKAGE_PLUGIN_PATH") or gst_root
    gst_plugin_path = path.join(gst_plugin_path_root, "lib", "gstreamer-1.0")
    if not os.path.exists(gst_plugin_path):
        print("ERROR: couldn't find gstreamer plugins at " + gst_plugin_path)
        return False

    missing = []
    for gst_lib in gst_dlls:
        try:
            shutil.copy(path.join(gst_plugin_path, gst_lib), servo_exe_dir)
        except Exception:
            missing += [str(gst_lib)]

    for gst_lib in missing:
        print("ERROR: could not find required GStreamer DLL: " + gst_lib)
    return not missing


def package_msvc_dlls(servo_exe_dir: str, target: BuildTarget) -> bool:
    def copy_file(dll_path: Union[PathLike[str], str]) -> bool:
        if not dll_path or not os.path.exists(dll_path):
            print(f"WARNING: Could not find DLL at {dll_path}", file=sys.stderr)
            return False
        servo_dir_dll = path.join(servo_exe_dir, os.path.basename(dll_path))
        # Avoid permission denied error when overwriting DLLs.
        if os.path.isfile(servo_dir_dll):
            os.chmod(servo_dir_dll, stat.S_IWUSR)
        print(f"    • Copying {dll_path}")
        shutil.copy(dll_path, servo_exe_dir)
        return True

    vs_platform = {
        "x86_64": "x64",
        "i686": "x86",
        "aarch64": "arm64",
    }[target.triple().split("-")[0]]

    for msvc_redist_dir in servo.visual_studio.find_msvc_redist_dirs(vs_platform):
        if copy_file(os.path.join(msvc_redist_dir, "msvcp140.dll")) and copy_file(
            os.path.join(msvc_redist_dir, "vcruntime140.dll")
        ):
            break

    # Different SDKs install the file into different directory structures within the
    # Windows SDK installation directory, so use a glob to search for a path like
    # "**\x64\api-ms-win-crt-runtime-l1-1-0.dll".
    windows_sdk_dir = servo.visual_studio.find_windows_sdk_installation_path()
    dll_name = "api-ms-win-crt-runtime-l1-1-0.dll"
    file_to_copy = next(pathlib.Path(windows_sdk_dir).rglob(os.path.join("**", vs_platform, dll_name)))
    copy_file(file_to_copy)

    return True
