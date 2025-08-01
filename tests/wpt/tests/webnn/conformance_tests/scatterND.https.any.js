// META: title=test WebNN API scatterND operation
// META: global=window
// META: variant=?cpu
// META: variant=?gpu
// META: variant=?npu
// META: script=../resources/utils.js
// META: timeout=long

'use strict';

const scatterNDTests = [
  {
    'name':
        'scatterND 1D float32 tensors (Insert individual elements in a tensor by index)',
    'graph': {
      'inputs': {
        'input': {
          'data': [1, 2, 3, 4, 5, 6, 7, 8],
          'descriptor': {shape: [8], dataType: 'float32'}
        },
        'indices': {
          'data': [4, 3, 1, 7],
          'descriptor': {shape: [4, 1], dataType: 'int32'}
        },
        'updates': {
          'data': [9, 10, 11, 12],
          'descriptor': {shape: [4], dataType: 'float32'}
        }
      },
      'operators': [{
        'name': 'scatterND',
        'arguments': [
          {'input': 'input'}, {'indices': 'indices'}, {'updates': 'updates'}
        ],
        'outputs': 'output'
      }],
      'expectedOutputs': {
        'output': {
          'data': [1, 11, 3, 10, 9, 6, 7, 12],
          'descriptor': {shape: [8], dataType: 'float32'}
        }
      }
    }
  },
  {
    'name':
        'scatterND 3D float32 tensors (Insert entire slices of a higher rank tensor)',
    'graph': {
      'inputs': {
        'input': {
          'data': [
            1, 2, 3, 4, 5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6,
            7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4,
            5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8
          ],
          'descriptor': {shape: [4, 4, 4], dataType: 'float32'}
        },
        'indices':
            {'data': [0, 2], 'descriptor': {shape: [2, 1], dataType: 'int32'}},
        'updates': {
          'data': [
            5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8,
            1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4
          ],
          'descriptor': {shape: [2, 4, 4], dataType: 'float32'}
        }
      },
      'operators': [{
        'name': 'scatterND',
        'arguments': [
          {'input': 'input'}, {'indices': 'indices'}, {'updates': 'updates'}
        ],
        'outputs': 'output'
      }],
      'expectedOutputs': {
        'output': {
          'data': [
            5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 1, 2, 3, 4, 5, 6,
            7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
            4, 4, 4, 4, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8
          ],
          'descriptor': {shape: [4, 4, 4], dataType: 'float32'}
        }
      }
    }
  },

  // float16 tests
  {
    'name':
        'scatterND 1D float16 tensors (Insert individual elements in a tensor by index)',
    'graph': {
      'inputs': {
        'input': {
          'data': [1, 2, 3, 4, 5, 6, 7, 8],
          'descriptor': {shape: [8], dataType: 'float16'}
        },
        'indices': {
          'data': [4, 3, 1, 7],
          'descriptor': {shape: [4, 1], dataType: 'int32'}
        },
        'updates': {
          'data': [9, 10, 11, 12],
          'descriptor': {shape: [4], dataType: 'float16'}
        }
      },
      'operators': [{
        'name': 'scatterND',
        'arguments': [
          {'input': 'input'}, {'indices': 'indices'}, {'updates': 'updates'}
        ],
        'outputs': 'output'
      }],
      'expectedOutputs': {
        'output': {
          'data': [1, 11, 3, 10, 9, 6, 7, 12],
          'descriptor': {shape: [8], dataType: 'float16'}
        }
      }
    }
  },
  {
    'name':
        'scatterND 3D float16 tensors (Insert entire slices of a higher rank tensor)',
    'graph': {
      'inputs': {
        'input': {
          'data': [
            1, 2, 3, 4, 5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6,
            7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4,
            5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8
          ],
          'descriptor': {shape: [4, 4, 4], dataType: 'float16'}
        },
        'indices':
            {'data': [0, 2], 'descriptor': {shape: [2, 1], dataType: 'int32'}},
        'updates': {
          'data': [
            5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8,
            1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4
          ],
          'descriptor': {shape: [2, 4, 4], dataType: 'float16'}
        }
      },
      'operators': [{
        'name': 'scatterND',
        'arguments': [
          {'input': 'input'}, {'indices': 'indices'}, {'updates': 'updates'}
        ],
        'outputs': 'output'
      }],
      'expectedOutputs': {
        'output': {
          'data': [
            5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 1, 2, 3, 4, 5, 6,
            7, 8, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
            4, 4, 4, 4, 8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8
          ],
          'descriptor': {shape: [4, 4, 4], dataType: 'float16'}
        }
      }
    }
  }
];

if (navigator.ml) {
  scatterNDTests.filter(isTargetTest).forEach((test) => {
    webnn_conformance_test(buildAndExecuteGraph, getZeroULPTolerance, test);
  });
} else {
  test(() => assert_implements(navigator.ml, 'missing navigator.ml'));
}
