// Purpose: Pipeline idempotence on symbolic dim algebra with deep-RAUW into all tensor types.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE1
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE2
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat

  %m0 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m1 = "dtensor.nat.mul"(%m0, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m2 = "dtensor.nat.add"(%m1, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %k0 = "dtensor.nat.add"(%k, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %k1 = "dtensor.nat.mul"(%k0, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %lhs = "dtensor.empty"() : () -> !dtensor.tensor<[%m2, %k1], f32>
  %rhs = "dtensor.empty"() : () -> !dtensor.tensor<[%k1, %n], f32>
  %mm = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m2, %k1], f32>, !dtensor.tensor<[%k1, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  %bias = "dtensor.empty"() : () -> !dtensor.tensor<[%m2, %n], f32>
  %sum = "dtensor.add"(%mm, %bias)
    : (!dtensor.tensor<[%m2, %n], f32>, !dtensor.tensor<[%m2, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  %prod = "dtensor.mul"(%sum, %sum)
    : (!dtensor.tensor<[%m2, %n], f32>, !dtensor.tensor<[%m2, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  %out = "dtensor.cast"(%prod)
    : (!dtensor.tensor<[%m2, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  "test.keep"(%out) : (!dtensor.tensor<[%m2, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %3 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// VERIFY:   %4 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// VERIFY:   %5 = "dtensor.nat.add"(%0, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %6 = "dtensor.nat.mul"(%5, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %7 = "dtensor.nat.add"(%6, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %8 = "dtensor.nat.add"(%1, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %9 = "dtensor.nat.mul"(%8, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %10 = "dtensor.empty"() : () -> !dtensor.tensor<[%7, %9], f32>
// VERIFY:   %11 = "dtensor.empty"() : () -> !dtensor.tensor<[%9, %2], f32>
// VERIFY:   %12 = "dtensor.matmul"(%10, %11) : (!dtensor.tensor<[%7, %9], f32>, !dtensor.tensor<[%9, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %13 = "dtensor.empty"() : () -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %14 = "dtensor.add"(%12, %13) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %15 = "dtensor.mul"(%14, %14) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %16 = "dtensor.cast"(%15) : (!dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   "test.keep"(%16) : (!dtensor.tensor<[%7, %2], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %3 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CANON:   %4 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CANON:   %5 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// CANON:   %6 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %2], f32>
// CANON:   %7 = "dtensor.matmul"(%5, %6) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%1, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %8 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %9 = "dtensor.add"(%7, %8) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %10 = "dtensor.mul"(%9, %9) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %11 = "dtensor.cast"(%10) : (!dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   "test.keep"(%11) : (!dtensor.tensor<[%0, %2], f32>) -> ()
// CANON: }

// PIPE1: builtin.module {
// PIPE1:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE1:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE1:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE1:   %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// PIPE1:   %4 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %2], f32>
// PIPE1:   %5 = "dtensor.matmul"(%3, %4) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%1, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE1:   %6 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %2], f32>
// PIPE1:   %7 = "dtensor.add"(%5, %6) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE1:   %8 = "dtensor.mul"(%7, %7) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE1:   %9 = "dtensor.cast"(%8) : (!dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE1:   "test.keep"(%9) : (!dtensor.tensor<[%0, %2], f32>) -> ()
// PIPE1: }

// PIPE2: builtin.module {
// PIPE2:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE2:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE2:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE2:   %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// PIPE2:   %4 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %2], f32>
// PIPE2:   %5 = "dtensor.matmul"(%3, %4) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%1, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE2:   %6 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %2], f32>
// PIPE2:   %7 = "dtensor.add"(%5, %6) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE2:   %8 = "dtensor.mul"(%7, %7) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE2:   %9 = "dtensor.cast"(%8) : (!dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE2:   "test.keep"(%9) : (!dtensor.tensor<[%0, %2], f32>) -> ()
// PIPE2: }
