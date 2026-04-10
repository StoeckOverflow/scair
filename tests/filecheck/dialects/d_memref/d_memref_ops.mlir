// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %m_i = "dtensor.shape.to_index"(%m) : (!dtensor.nat) -> index
  %n_i = "dtensor.shape.to_index"(%n) : (!dtensor.nat) -> index
  %z_i = "arith.constant"() <{value = 0 : index}> : () -> index
  %o_i = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
  %v = "test.v"() : () -> f32
  d_memref.store %v, %buf[%o_i, %o_i] : f32, !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
  %r = d_memref.load %buf[%o_i, %o_i] : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> f32
  %d0 = d_memref.dim %buf, %z_i : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> index
  %dx = d_memref.dim_exact %buf {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> !value<%m>
  %sv = d_memref.subview %buf[%z_i, %z_i][%m_i, %n_i][%o_i, %o_i] : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> !d_memref.memref<[%m, %n], f32>
  %c = d_memref.cast %sv : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%m, %n], f32>
  %rc = d_memref.reinterpret_cast %buf : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> to !d_memref.memref<[%m, %n], f32, offset: %z_i, strides: [%n_i, %o_i]>
  %vx = "test.vx"() : () -> !d_memref.vector<%m, f32>
  %mx = "test.mx"() : () -> !d_memref.matrix<%m, %n, f32>
  "test.keep"(%r, %d0, %dx, %c, %rc, %vx, %mx) : (f32, index, !value<%m>, !d_memref.memref<[%m, %n], f32>, !d_memref.memref<[%m, %n], f32, offset: %z_i, strides: [%n_i, %o_i]>, !d_memref.vector<%m, f32>, !d_memref.matrix<%m, %n, f32>) -> ()
  d_memref.dealloc %buf : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
}

// VERIFY: d_memref.dim %{{.*}}, %{{.*}} : !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: 0, strides: [%{{.*}}, 1]> -> index
// VERIFY: d_memref.dim_exact %{{.*}} {axis = 0 : i32} : !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: 0, strides: [%{{.*}}, 1]> -> !value<%{{.*}}>
// VERIFY: d_memref.subview %{{.*}}[%{{.*}}, %{{.*}}][%{{.*}}, %{{.*}}][%{{.*}}, %{{.*}}]
// VERIFY: d_memref.reinterpret_cast %{{.*}}

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.dim_exact %buf {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32> -> !value<%n>
}

// VERIFY: d_memref.dim_exact: expected result !value<...> to reference the selected embedded dim

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.load %buf[%o] : !d_memref.memref<[%m, %n], f32> -> f32
}

// VERIFY: d_memref.load: expected 2 indices, got 1

// -----

builtin.module {
  %m0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %m1 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m0], f32>
  %bad = d_memref.cast %buf : !d_memref.memref<[%m0], f32> -> !d_memref.memref<[%m1], f32>
}

// VERIFY: d_memref.cast: expected pairwise SSA-identical dims

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m], f32, offset: 0, strides: [1]>
  %bad = d_memref.cast %buf : !d_memref.memref<[%m], f32, offset: 0, strides: [1]> -> !d_memref.memref<[%m], f32, offset: 1, strides: [1]>
}

// VERIFY: d_memref.cast: expected identical layout metadata

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %m_i = "dtensor.shape.to_index"(%m) : (!dtensor.nat) -> index
  %n_i = "dtensor.shape.to_index"(%n) : (!dtensor.nat) -> index
  %z_i = "arith.constant"() <{value = 0 : index}> : () -> index
  %o_i = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.subview %buf[%z_i, %z_i][%m_i, %n_i][%o_i, %o_i] : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%n, %m], f32>
}

// VERIFY: d_memref.subview: size provenance mismatch at axis 0; expected result dim to match size operand via dtensor.shape.to_index

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.reinterpret_cast %buf : !d_memref.memref<[%m, %n], f32> to !d_memref.memref<[%n, %m], f32>
}

// VERIFY: d_memref.reinterpret_cast: expected destination type to encode offset and strides

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %two = "arith.constant"() <{value = 2 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.dim %buf, %two : !d_memref.memref<[%m, %n], f32> -> index
}

// VERIFY: d_memref.dim: constant axis 2 out of bounds for rank 2

// -----

builtin.module {
  %z_i = "arith.constant"() <{value = 0 : index}> : () -> index
  %o_i = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
  %v = "test.v"() : () -> f32
  d_memref.store %v, %buf[%o_i, %o_i] : f32, !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
  %r = d_memref.load %buf[%o_i, %o_i] : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> f32
  %d0 = d_memref.dim %buf, %z_i : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> index
  %same = d_memref.cast %buf : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
  "test.keep"(%r, %d0, %same) : (f32, index, !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>) -> ()
  d_memref.dealloc %buf : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
}

// VERIFY: d_memref.alloc : () -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
// VERIFY: d_memref.cast %{{.*}} : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m], f32>
  %bad = d_memref.cast %buf : !d_memref.memref<[%m], f32> -> !d_memref.memref<[4], f32>
}

// VERIFY: d_memref.cast: expected pairwise SSA-identical dims

// -----

builtin.module {
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32>
  %bad = d_memref.dim_exact %buf {axis = 0 : i32} : !d_memref.memref<[4], f32> -> !value<%buf>
}

// VERIFY: d_memref.dim_exact: expected selected embedded dim to be SSA-backed, got a literal dimension
