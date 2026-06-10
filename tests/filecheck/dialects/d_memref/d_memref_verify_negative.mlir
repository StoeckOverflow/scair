// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY

builtin.module {
  %x = "test.float_dim"() : () -> f32
  %bad = d_memref.alloc : () -> !d_memref.memref<[%x], f32>
}

// VERIFY: shape SSA parameter must have type !d_tensor.nat or !d_tensor.posnat, got f32

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %layout = "test.float_layout"() : () -> f32
  %bad = d_memref.alloc : () -> !d_memref.memref<[%m], f32, offset: %layout, strides: [1]>
}

// VERIFY: layout SSA parameter must have type index, integer, !d_tensor.nat, !d_tensor.posnat, or !value<...>, got f32

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %bad = d_memref.alloc : () -> !d_memref.memref<[%m], f32, offset: 0, strides: [1, 1]>
}

// VERIFY: d_memref.memref: expected 1 strides for rank 1, got 2

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %axis = "arith.constant"() <{value = -1 : index}> : () -> index
  %bad = d_memref.dim %buf, %axis : !d_memref.memref<[4], f32> -> index
}

// VERIFY: d_memref.dim: constant axis -1 out of bounds for rank 1

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %axis = "arith.constant"() <{value = 0 : index}> : () -> index
  %bad = "d_memref.dim"(%buf, %axis) : (!d_memref.memref<[4], f32>, index) -> i32
}

// VERIFY: d_memref.dim: expected result type index, got i32

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.dim_exact %buf {axis = 0 : i64} : !d_memref.memref<[4], f32> -> !value<%buf>
}

// VERIFY: d_memref.dim_exact: expected i32 axis attribute, got i64

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.dim_exact %buf {axis = 1 : i32} : !d_memref.memref<[4], f32> -> !value<%buf>
}

// VERIFY: d_memref.dim_exact: axis 1 out of bounds for rank 1

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %idx = "arith.constant"() <{value = 0 : index}> : () -> index
  %bad = d_memref.load %buf[%idx] : !d_memref.memref<[4], f32> -> i32
}

// VERIFY: d_memref.load: expected result type f32, got i32

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %idx = "arith.constant"() <{value = 0 : index}> : () -> index
  %v = "test.value"() : () -> i32
  d_memref.store %v, %buf[%idx] : i32, !d_memref.memref<[4], f32>
}

// VERIFY: d_memref.store: expected stored value type f32, got i32

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %idx = "arith.constant"() <{value = 0 : index}> : () -> index
  %v = "test.value"() : () -> f32
  d_memref.store %v, %buf[%idx, %idx] : f32, !d_memref.memref<[4], f32>
}

// VERIFY: d_memref.store: expected 1 indices, got 2

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %base, %offset = "d_memref.extract_strided_metadata"(%buf)
    : (!d_memref.memref<[4], f32>) -> (!d_memref.memref<[], f32>, index)
}

// VERIFY: d_memref.extract_strided_metadata: expected 4 results for rank 1, got 2

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %base, %offset, %size0, %stride0 = "d_memref.extract_strided_metadata"(%buf)
    : (!d_memref.memref<[4], f32>) -> (!d_memref.memref<[], i32>, index, index, index)
}

// VERIFY: d_memref.extract_strided_metadata: result 0 expected type !d_memref.memref<[], f32>, got !d_memref.memref<[], i32>

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %base, %offset, %size0, %stride0 = "d_memref.extract_strided_metadata"(%buf)
    : (!d_memref.memref<[4], f32>) -> (!d_memref.memref<[], f32>, i32, index, index)
}

// VERIFY: d_memref.extract_strided_metadata: result 1 expected type index, got i32

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.cast %buf : !d_memref.memref<[4], f32> -> !d_memref.memref<[4], i32>
}

// VERIFY: d_memref.cast: expected equal element types, got f32 and i32

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.cast %buf : !d_memref.memref<[4], f32> -> !d_memref.memref<[4, 1], f32>
}

// VERIFY: d_memref.cast: expected equal ranks, got 1 and 2

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %idx = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %bad = d_memref.subview %buf[%idx, %idx][%one][%one] : !d_memref.memref<[4], f32> -> !d_memref.memref<[1], f32>
}

// VERIFY: d_memref.subview: expected 1 offsets, got 2

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %idx = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %bad = d_memref.subview %buf[%idx][%one][%one] : !d_memref.memref<[4], f32> -> !d_memref.memref<[1], i32>
}

// VERIFY: d_memref.subview: expected equal element types, got f32 and i32

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %idx = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %bad = d_memref.subview %buf[%idx][%one][%one] : !d_memref.memref<[4], f32> -> !d_memref.memref<[], f32>
}

// Restricted subset: rank-reducing d_memref.subview is not supported.
// VERIFY: d_memref.subview: expected equal source/result rank, got 1 and 0

// -----

builtin.module {
  %neg = "arith.constant"() <{value = -1 : index}> : () -> index
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.subview %buf[%neg][%zero][%one] : !d_memref.memref<[4], f32> -> !d_memref.memref<[0], f32>
}

// VERIFY: d_memref.subview: expected non-negative static offset at axis 0, got -1

// -----

builtin.module {
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %neg = "arith.constant"() <{value = -1 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.subview %buf[%zero][%neg][%one] : !d_memref.memref<[4], f32> -> !d_memref.memref<[0], f32>
}

// VERIFY: d_memref.subview: expected non-negative static size at axis 0, got -1

// -----

builtin.module {
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.subview %buf[%zero][%one][%zero] : !d_memref.memref<[4], f32> -> !d_memref.memref<[1], f32>
}

// VERIFY: d_memref.subview: expected positive static stride at axis 0, got 0

// -----

builtin.module {
  %five = "arith.constant"() <{value = 5 : index}> : () -> index
  %four = "arith.constant"() <{value = 4 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[8], f32>
  %bad = d_memref.subview %buf[%five][%four][%one] : !d_memref.memref<[8], f32> -> !d_memref.memref<[4], f32>
}

// VERIFY: d_memref.subview: static slice at axis 0 is out of bounds for dimension 8 (offset 5, size 4, stride 1)

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.reinterpret_cast %buf : !d_memref.memref<[4], f32> to !d_memref.memref<[4], i32, offset: 0, strides: [1]>
}

// VERIFY: d_memref.reinterpret_cast: expected equal element types, got f32 and i32

// -----

builtin.module {
  %bad = d_memref.alloc : () -> !d_memref.memref<[-1], f32>
}

// VERIFY: d_memref: expected non-negative static dimension, got -1

// -----

builtin.module {
  %bad = d_memref.alloc : () -> !d_memref.memref<[4], f32, offset: -1, strides: [1]>
}

// VERIFY: d_memref.memref: expected non-negative static offset, got -1

// -----

builtin.module {
  %bad = d_memref.alloc : () -> !d_memref.memref<[4], f32, offset: 0, strides: [0]>
}

// VERIFY: d_memref.memref: expected positive static stride, got 0

// -----

builtin.module {
  %bad = d_memref.alloc : () -> !d_memref.memref<[4], f32, offset: 0, strides: [-2]>
}

// VERIFY: d_memref.memref: expected positive static stride, got -2

// -----

builtin.module {
  %o0 = "arith.constant"() <{value = 1 : index}> : () -> index
  %o1 = "arith.constant"() <{value = 2 : index}> : () -> index
  %s0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %s1 = "arith.constant"() <{value = 3 : index}> : () -> index
  %st0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %st1 = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
  %bad = d_memref.subview %buf[%o0, %o1][%s0, %s1][%st0, %st1]
    : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
      -> !d_memref.memref<[2, 3], f32, offset: 9, strides: [16, 1]>
}

// VERIFY: d_memref.subview: result offset mismatch; expected 10, got 9

// -----

builtin.module {
  %off = "test.offset"() : () -> index
  %stride = "test.stride"() : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %two = "arith.constant"() <{value = 2 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32, offset: %off, strides: [%stride]>
  %bad = d_memref.subview %buf[%one][%two][%one]
    : !d_memref.memref<[4], f32, offset: %off, strides: [%stride]>
      -> !d_memref.memref<[2], f32, offset: %off, strides: [%stride]>
}

// VERIFY: d_memref.subview: explicit result layout is outside the restricted verified subset unless it is statically derivable or an identity dynamic slice

// -----

builtin.module {
  %o0 = "arith.constant"() <{value = 1 : index}> : () -> index
  %o1 = "arith.constant"() <{value = 2 : index}> : () -> index
  %s0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %s1 = "arith.constant"() <{value = 3 : index}> : () -> index
  %st0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %st1 = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
  %bad = d_memref.subview %buf[%o0, %o1][%s0, %s1][%st0, %st1]
    : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
      -> !d_memref.memref<[2, 3], f32, offset: 10, strides: [15, 1]>
}

// VERIFY: d_memref.subview: result stride mismatch at axis 0; expected 16, got 15

// -----

builtin.module {
  %off = "test.offset"() : () -> index
  %other_off = "test.other_offset"() : () -> index
  %stride = "test.stride"() : () -> index
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %two = "arith.constant"() <{value = 2 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32, offset: %off, strides: [%stride]>
  %bad = d_memref.subview %buf[%zero][%two][%one]
    : !d_memref.memref<[4], f32, offset: %off, strides: [%stride]>
      -> !d_memref.memref<[2], f32, offset: %other_off, strides: [%stride]>
}

// VERIFY: d_memref.subview: explicit result layout is outside the restricted verified subset unless it is statically derivable or an identity dynamic slice
