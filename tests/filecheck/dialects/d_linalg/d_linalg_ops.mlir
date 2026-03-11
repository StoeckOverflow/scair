// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %t = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], i32>
  %r = "d_linalg.fill"(%v, %t) : (i32, !dtensor.tensor<[%m, %n], i32>) -> !dtensor.tensor<[%m, %n], i32>
  "test.keep"(%r) : (!dtensor.tensor<[%m, %n], i32>) -> ()
}

// VERIFY: "d_linalg.fill"

// -----

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %k], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%k, %n], f32>
  %out = "test.out"() : () -> !dtensor.tensor<[%m, %n], f32>
  %r = "d_linalg.matmul"(%a, %b, %out) : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%r) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY: "d_linalg.matmul"

// -----

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  "d_linalg.fill"(%v, %buf) : (i32, !d_memref.memref<[%m, %n], i32>) -> ()
}

// VERIFY: "d_linalg.fill"

// -----

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  %bad = "d_linalg.fill"(%v, %buf) : (i32, !d_memref.memref<[%m, %n], i32>) -> !d_memref.memref<[%m, %n], i32>
  "test.keep"(%bad) : (!d_memref.memref<[%m, %n], i32>) -> ()
}

// VERIFY: d_linalg.fill: memref form expects 0 results, got 1
