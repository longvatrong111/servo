/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use app_units::Au;
use fonts::platform::font::PlatformFont;
use fonts::{
    Font, FontData, FontDescriptor, FontIdentifier, FontTemplate, FontTemplateRef,
    PlatformFontMethods, ShapingFlags, ShapingOptions,
};
use icu_locale_core::subtags::Language;
use servo_url::ServoUrl;
use skrifa::MetadataProvider;
use skrifa::prelude::{LocationRef, Size};
use style::computed_values::font_optical_sizing::T as FontOpticalSizing;
use style::computed_values::font_variant_position::T as FontVariantPosition;
use style::properties::longhands::font_variant_caps::computed_value::T as FontVariantCaps;
use style::values::computed::{
    FontFeatureSettings, FontStretch, FontStyle, FontSynthesis, FontVariantEastAsian,
    FontVariantLigatures, FontVariantNumeric, FontWeight,
};
use unicode_script::Script;

fn make_font(path: PathBuf) -> Font {
    let mut bytes = Vec::new();
    File::open(path.clone())
        .expect("Couldn't open font file!")
        .read_to_end(&mut bytes)
        .unwrap();
    let data = FontData::from_bytes(&bytes);

    let identifier = FontIdentifier::Web(ServoUrl::from_file_path(path).unwrap());
    let platform_font =
        PlatformFont::new_from_data(identifier.clone(), &data, None, false).unwrap();

    let template = FontTemplate::new(identifier, platform_font.descriptor(), None);
    let descriptor = FontDescriptor {
        weight: FontWeight::normal(),
        stretch: FontStretch::hundred(),
        style: FontStyle::normal(),
        variant: FontVariantCaps::Normal,
        pt_size: Au::from_px(24),
        variation_settings: vec![],
        synthesis_weight: FontSynthesis::Auto,
        optical_sizing: FontOpticalSizing::Auto,
    };
    Font::new(FontTemplateRef::new(template), descriptor, Some(data), None).unwrap()
}

fn rasterization_test_font() -> Font {
    make_font(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/support/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf"),
    )
}

#[test]
fn test_rasterize_glyph_coverage_and_bearings() {
    let font = rasterization_test_font();
    for character in ['A', 'O', 'j', 'g'] {
        let glyph = font
            .rasterize_glyph(font.glyph_index(character).unwrap(), 32.0)
            .unwrap();
        assert!(glyph.width > 0 && glyph.height > 0);
        assert_eq!(glyph.coverage.len(), (glyph.width * glyph.height) as usize);
        assert!(glyph.coverage.contains(&0));
        assert!(glyph.coverage.contains(&255));
        assert!(glyph.coverage.iter().any(|&value| value > 0 && value < 255));
        assert!(glyph.top < 0, "ascenders must lie above the baseline");
        if character == 'j' {
            assert!(glyph.left < 0, "negative left bearings must be preserved");
        }
        if character == 'g' {
            assert!(glyph.top + glyph.height as i32 > 0, "descenders must lie below the baseline");
        }
        if character == 'O' {
            let center = ((glyph.height / 2) * glyph.width + glyph.width / 2) as usize;
            assert_eq!(glyph.coverage[center], 0, "glyph counters must remain empty");
        }
    }
}

#[test]
fn test_rasterize_glyph_uses_pixels_per_em() {
    let font = rasterization_test_font();
    let data = font.font_data_and_index().ok().expect("Missing test font data");
    let face = skrifa::FontRef::from_index(data.data.as_ref(), data.index).unwrap();
    // E has only straight edges, so its control bounds equal its ink bounds.
    let glyph_id = font.glyph_index('E').unwrap();
    for size in [16.0, 32.0, 47.5] {
        let bounds = face
            .glyph_metrics(Size::new(size), LocationRef::default())
            .bounds(skrifa::GlyphId::new(glyph_id))
            .unwrap();
        let glyph = font.rasterize_glyph(glyph_id, size).unwrap();
        assert_eq!(glyph.left, bounds.x_min.floor() as i32);
        assert_eq!(glyph.top, (-bounds.y_max).floor() as i32);
        assert_eq!(glyph.width, (bounds.x_max.ceil() - bounds.x_min.floor()) as u32);
        assert_eq!(glyph.height, ((-bounds.y_min).ceil() - (-bounds.y_max).floor()) as u32);
    }
}

#[test]
fn test_rasterize_glyph_empty_and_invalid_inputs() {
    let font = rasterization_test_font();
    assert!(font.rasterize_glyph(font.glyph_index(' ').unwrap(), 32.0).is_none());
    assert!(font.rasterize_glyph(u32::MAX, 32.0).is_none());
    let glyph_id = font.glyph_index('A').unwrap();
    for size in [0.0, -1.0, f32::NAN, f32::INFINITY, f32::NEG_INFINITY, f32::MAX] {
        assert!(font.rasterize_glyph(glyph_id, size).is_none());
    }
}

#[test]
fn test_rasterize_glyph_reuses_font_data() {
    let font = rasterization_test_font();
    let glyph_id = font.glyph_index('A').unwrap();
    let first = font.rasterize_glyph(glyph_id, 24.0).unwrap();
    let second = font.rasterize_glyph(glyph_id, 24.0).unwrap();
    assert_eq!((first.left, first.top), (second.left, second.top));
    assert_eq!((first.width, first.height), (second.width, second.height));
    assert_eq!(first.coverage, second.coverage);
}

#[test]
fn test_font_can_do_fast_shaping() {
    let dejavu_sans = make_font(
        [
            env!("CARGO_MANIFEST_DIR"),
            "tests",
            "support",
            "dejavu-fonts-ttf-2.37",
            "ttf",
            "DejaVuSans.ttf",
        ]
        .iter()
        .collect(),
    );

    let dejavu_sans_fast_shapeable = make_font(
        [
            env!("CARGO_MANIFEST_DIR"),
            "tests",
            "support",
            "dejavu-fonts-ttf-2.37",
            "ttf",
            "DejaVuSansNoGSUBNoGPOS.ttf",
        ]
        .iter()
        .collect(),
    );

    // Fast shaping requires a font with a kern table and no GPOS or GSUB tables.
    let shaping_options = ShapingOptions {
        letter_spacing: None,
        word_spacing: None,
        script: Script::Latin,
        language: Language::UNKNOWN,
        flags: ShapingFlags::empty(),
        ligatures: FontVariantLigatures::NORMAL,
        numeric: FontVariantNumeric::NORMAL,
        east_asian: FontVariantEastAsian::NORMAL,
        feature_settings: FontFeatureSettings::normal(),
        position: FontVariantPosition::Normal,
        alternates: Default::default(),
    };
    assert!(!dejavu_sans.can_do_fast_shaping("WAVE", &shaping_options));
    assert!(dejavu_sans_fast_shapeable.can_do_fast_shaping("WAVE", &shaping_options));

    // Non-Latin script should never have fast shaping.
    let shaping_options = ShapingOptions {
        letter_spacing: None,
        word_spacing: None,
        script: Script::Cherokee,
        language: Language::UNKNOWN,
        flags: ShapingFlags::empty(),
        ligatures: FontVariantLigatures::NORMAL,
        numeric: FontVariantNumeric::NORMAL,
        east_asian: FontVariantEastAsian::NORMAL,
        feature_settings: FontFeatureSettings::normal(),
        position: FontVariantPosition::Normal,
        alternates: Default::default(),
    };
    assert!(!dejavu_sans.can_do_fast_shaping("WAVE", &shaping_options));
    assert!(!dejavu_sans_fast_shapeable.can_do_fast_shaping("WAVE", &shaping_options));

    // Right-to-left text should never use fast shaping.
    let shaping_options = ShapingOptions {
        letter_spacing: None,
        word_spacing: None,
        script: Script::Latin,
        language: Language::UNKNOWN,
        flags: ShapingFlags::RTL_FLAG,
        ligatures: FontVariantLigatures::NORMAL,
        numeric: FontVariantNumeric::NORMAL,
        east_asian: FontVariantEastAsian::NORMAL,
        feature_settings: FontFeatureSettings::normal(),
        position: FontVariantPosition::Normal,
        alternates: Default::default(),
    };
    assert!(!dejavu_sans.can_do_fast_shaping("WAVE", &shaping_options));
    assert!(!dejavu_sans_fast_shapeable.can_do_fast_shaping("WAVE", &shaping_options));
}
