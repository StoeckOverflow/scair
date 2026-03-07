// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %v = "test.v"() : () -> f32
  d_memref.store %v, %buf[%o, %o] : f32, !d_memref.memref<[%m, %n], f32>
  %r = d_memref.load %buf[%o, %o] : !d_memref.memref<[%m, %n], f32> -> f32
  %d0 = d_memref.dim %buf {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32> -> !value<%m>
  %sv = d_memref.subview %buf[%z, %z][%m, %n] : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%m, %n], f32>
  %c = d_memref.cast %sv : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%m, %n], f32>
  %vx = "test.vx"() : () -> !d_memref.vector<%m, f32>
  %mx = "test.mx"() : () -> !d_memref.matrix<%m, %n, f32>
  "test.keep"(%r, %d0, %c, %vx, %mx) : (f32, !value<%m>, !d_memref.memref<[%m, %n], f32>, !d_memref.vector<%m, f32>, !d_memref.matrix<%m, %n, f32>) -> ()
  d_memref.dealloc %buf : !d_memref.memref<[%m, %n], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// VERIFY:   %3 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// VERIFY:   %4 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], f32>
// VERIFY:   %5 = "test.v"() : () -> f32
// VERIFY:   d_memref.store %5, %4[%3, %3] : f32, !d_memref.memref<[%0, %1], f32>
// VERIFY:   %6 = d_memref.load %4[%3, %3] : !d_memref.memref<[%0, %1], f32> -> f32
// VERIFY:   %7 = d_memref.dim %4 {axis = 0 : i32} : !d_memref.memref<[%0, %1], f32> -> !value<%0>
// VERIFY:   %8 = d_memref.subview %4[%2, %2][%0, %1] : !d_memref.memref<[%0, %1], f32> -> !d_memref.memref<[%0, %1], f32>
// VERIFY:   %9 = d_memref.cast %8 : !d_memref.memref<[%0, %1], f32> -> !d_memref.memref<[%0, %1], f32>
// VERIFY:   %10 = "test.vx"() : () -> !d_memref.vector<%0, f32>
// VERIFY:   %11 = "test.mx"() : () -> !d_memref.matrix<%0, %1, f32>
// VERIFY:   "test.keep"(%6, %7, %9, %10, %11) : (f32, !value<%0>, !d_memref.memref<[%0, %1], f32>, !d_memref.vector<%0, f32>, !d_memref.matrix<%0, %1, f32>) -> ()
// VERIFY:   d_memref.dealloc %4 : !d_memref.memref<[%0, %1], f32>
// VERIFY: }

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.dim %buf {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32> -> !value<%n>
}

// VERIFY: d_memref.dim: expected result !value<...> to reference the selected embedded dim

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
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
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32>
  %bad = d_memref.subview %buf[%z, %z][%m, %n] : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%n, %m], f32>
}

// VERIFY: d_memref.subview: expected result dims to be exactly the size operands
