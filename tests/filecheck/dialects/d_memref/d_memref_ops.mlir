// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %m_i = "d_tensor.shape.to_index"(%m) : (!d_tensor.nat) -> index
  %n_i = "d_tensor.shape.to_index"(%n) : (!d_tensor.nat) -> index
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

// VERIFY: builtin.module {
// VERIFY-NEXT:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// VERIFY-NEXT:   %1 = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// VERIFY-NEXT:   %2 = "d_tensor.shape.to_index"(%0) : (!d_tensor.nat) -> index
// VERIFY-NEXT:   %3 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// VERIFY-NEXT:   %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// VERIFY-NEXT:   %5 = "arith.constant"() <{value = 1 : index}> : () -> index
// VERIFY-NEXT:   %6 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]>
// VERIFY-NEXT:   %7 = "test.v"() : () -> f32
// VERIFY-NEXT:   d_memref.store %7, %6[%5, %5] : f32, !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]>
// VERIFY-NEXT:   %8 = d_memref.load %6[%5, %5] : !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]> -> f32
// VERIFY-NEXT:   %9 = d_memref.dim %6, %4 : !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]> -> index
// VERIFY-NEXT:   %10 = d_memref.dim_exact %6 {axis = 0 : i32} : !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]> -> !value<%0>
// VERIFY-NEXT:   %11 = d_memref.subview %6[%4, %4][%2, %3][%5, %5] : !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]> -> !d_memref.memref<[%0, %1], f32>
// VERIFY-NEXT:   %12 = d_memref.cast %11 : !d_memref.memref<[%0, %1], f32> -> !d_memref.memref<[%0, %1], f32>
// VERIFY-NEXT:   %13 = d_memref.reinterpret_cast %6
// VERIFY-NEXT:   : !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]> to !d_memref.memref<[%0, %1], f32, offset: %4, strides: [%3, %5]>
// VERIFY-NEXT:   %14 = "test.vx"() : () -> !d_memref.vector<%0, f32>
// VERIFY-NEXT:   %15 = "test.mx"() : () -> !d_memref.matrix<%0, %1, f32>
// VERIFY-NEXT:   "test.keep"(%8, %9, %10, %12, %13, %14, %15) : (f32, index, !value<%0>, !d_memref.memref<[%0, %1], f32>, !d_memref.memref<[%0, %1], f32, offset: %4, strides: [%3, %5]>, !d_memref.vector<%0, f32>, !d_memref.matrix<%0, %1, f32>) -> ()
// VERIFY-NEXT:   d_memref.dealloc %6 : !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%1, 1]>
// VERIFY-NEXT: }

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.dim_exact %buf {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32> -> !value<%n>
}

// VERIFY: d_memref.dim_exact: expected result !value<...> to reference the selected embedded dim

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.load %buf[%o] : !d_memref.memref<[%m, %n], f32> -> f32
}

// VERIFY: d_memref.load: expected 2 indices, got 1

// -----

builtin.module {
  %m0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %m1 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m0], f32>
  %bad = d_memref.cast %buf : !d_memref.memref<[%m0], f32> -> !d_memref.memref<[%m1], f32>
}

// VERIFY: d_memref.cast: expected pairwise SSA-identical dims

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m], f32, offset: 0, strides: [1]>
  %bad = d_memref.cast %buf : !d_memref.memref<[%m], f32, offset: 0, strides: [1]> -> !d_memref.memref<[%m], f32, offset: 1, strides: [1]>
}

// VERIFY: d_memref.cast: expected identical layout metadata

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %m_i = "d_tensor.shape.to_index"(%m) : (!d_tensor.nat) -> index
  %n_i = "d_tensor.shape.to_index"(%n) : (!d_tensor.nat) -> index
  %z_i = "arith.constant"() <{value = 0 : index}> : () -> index
  %o_i = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.subview %buf[%z_i, %z_i][%m_i, %n_i][%o_i, %o_i] : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%n, %m], f32>
}

// VERIFY: d_memref.subview: size provenance mismatch at axis 0; expected result dim to match size operand via d_tensor.shape.to_index

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.reinterpret_cast %buf : !d_memref.memref<[%m, %n], f32> to !d_memref.memref<[%n, %m], f32>
}

// VERIFY: d_memref.reinterpret_cast: expected destination type to encode offset and strides

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
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

// VERIFY: builtin.module {
// VERIFY-NEXT:   %0 = "arith.constant"() <{value = 0 : index}> : () -> index
// VERIFY-NEXT:   %1 = "arith.constant"() <{value = 1 : index}> : () -> index
// VERIFY-NEXT:   %2 = d_memref.alloc : () -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
// VERIFY-NEXT:   %3 = "test.v"() : () -> f32
// VERIFY-NEXT:   d_memref.store %3, %2[%1, %1] : f32, !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
// VERIFY-NEXT:   %4 = d_memref.load %2[%1, %1] : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> f32
// VERIFY-NEXT:   %5 = d_memref.dim %2, %0 : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> index
// VERIFY-NEXT:   %6 = d_memref.cast %2 : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
// VERIFY-NEXT:   "test.keep"(%4, %5, %6) : (f32, index, !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>) -> ()
// VERIFY-NEXT:   d_memref.dealloc %2 : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
// VERIFY-NEXT: }

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
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

// -----

builtin.module {
  %o0 = "arith.constant"() <{value = 1 : index}> : () -> index
  %o1 = "arith.constant"() <{value = 2 : index}> : () -> index
  %s0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %s1 = "arith.constant"() <{value = 3 : index}> : () -> index
  %st0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %st1 = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
  %sv = d_memref.subview %buf[%o0, %o1][%s0, %s1][%st0, %st1]
    : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]>
      -> !d_memref.memref<[2, 3], f32, offset: 10, strides: [16, 1]>
  "test.keep"(%sv) : (!d_memref.memref<[2, 3], f32, offset: 10, strides: [16, 1]>) -> ()
}

// VERIFY: d_memref.subview %{{[0-9]+}}[%{{[0-9]+}}, %{{[0-9]+}}][%{{[0-9]+}}, %{{[0-9]+}}][%{{[0-9]+}}, %{{[0-9]+}}] : !d_memref.memref<[4, 8], f32, offset: 0, strides: [8, 1]> -> !d_memref.memref<[2, 3], f32, offset: 10, strides: [16, 1]>

// -----

builtin.module {
  %off = "test.offset"() : () -> index
  %stride = "test.stride"() : () -> index
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %two = "arith.constant"() <{value = 2 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[4], f32, offset: %off, strides: [%stride]>
  %sv = d_memref.subview %buf[%zero][%two][%one]
    : !d_memref.memref<[4], f32, offset: %off, strides: [%stride]>
      -> !d_memref.memref<[2], f32, offset: %off, strides: [%stride]>
  "test.keep"(%sv) : (!d_memref.memref<[2], f32, offset: %off, strides: [%stride]>) -> ()
}

// VERIFY: d_memref.subview %{{[0-9]+}}[%{{[0-9]+}}][%{{[0-9]+}}][%{{[0-9]+}}] : !d_memref.memref<[4], f32, offset: %{{[0-9]+}}, strides: [%{{[0-9]+}}]> -> !d_memref.memref<[2], f32, offset: %{{[0-9]+}}, strides: [%{{[0-9]+}}]>
