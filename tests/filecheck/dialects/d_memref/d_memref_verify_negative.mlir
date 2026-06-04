// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY

builtin.module {
  %x = "test.float_dim"() : () -> f32
  %bad = d_memref.alloc : () -> !d_memref.memref<[%x], f32>
}

// VERIFY: shape SSA parameter must have type !dtensor.nat or !dtensor.posnat, got f32

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %layout = "test.float_layout"() : () -> f32
  %bad = d_memref.alloc : () -> !d_memref.memref<[%m], f32, offset: %layout, strides: [1]>
}

// VERIFY: layout SSA parameter must have type index, integer, !dtensor.nat, !dtensor.posnat, or !value<...>, got f32

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
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
  %bad = d_memref.reinterpret_cast %buf : !d_memref.memref<[4], f32> to !d_memref.memref<[4], i32, offset: 0, strides: [1]>
}

// VERIFY: d_memref.reinterpret_cast: expected equal element types, got f32 and i32
