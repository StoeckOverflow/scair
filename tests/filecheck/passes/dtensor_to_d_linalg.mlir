// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p lower-dtensor-to-d-linalg | filecheck %s -DFILE=%s --check-prefix=LOWER

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %k], i32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%k, %n], i32>
  %c = "dtensor.matmul"(%a, %b) : (!dtensor.tensor<[%m, %k], i32>, !dtensor.tensor<[%k, %n], i32>) -> !dtensor.tensor<[%m, %n], i32>
  "test.keep"(%c) : (!dtensor.tensor<[%m, %n], i32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER: %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER: %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], i32>
// LOWER: %4 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %2], i32>
// LOWER: %5 = "arith.constant"() <{value = 0 : i32}> : () -> i32
// LOWER: %6 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %2], i32>
// LOWER: %7 = "d_linalg.fill"(%5, %6) : (i32, !dtensor.tensor<[%0, %2], i32>) -> !dtensor.tensor<[%0, %2], i32>
// LOWER: %8 = "d_linalg.matmul"(%3, %4, %7) : (!dtensor.tensor<[%0, %1], i32>, !dtensor.tensor<[%1, %2], i32>, !dtensor.tensor<[%0, %2], i32>) -> !dtensor.tensor<[%0, %2], i32>
// LOWER-NOT: "dtensor.matmul"
