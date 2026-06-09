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
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %o = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat

  %m0 = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %m1 = "d_tensor.nat.mul"(%m0, %o) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %m2 = "d_tensor.nat.add"(%m1, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  %k0 = "d_tensor.nat.add"(%k, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %k1 = "d_tensor.nat.mul"(%k0, %o) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  %lhs = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m2, %k1], f32>
  %rhs = "d_tensor.empty"() : () -> !d_tensor.tensor<[%k1, %n], f32>
  %mm = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m2, %k1], f32>, !d_tensor.tensor<[%k1, %n], f32>) -> !d_tensor.tensor<[%m2, %n], f32>
  %bias = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m2, %n], f32>
  %sum = "d_tensor.add"(%mm, %bias)
    : (!d_tensor.tensor<[%m2, %n], f32>, !d_tensor.tensor<[%m2, %n], f32>) -> !d_tensor.tensor<[%m2, %n], f32>
  %prod = "d_tensor.mul"(%sum, %sum)
    : (!d_tensor.tensor<[%m2, %n], f32>, !d_tensor.tensor<[%m2, %n], f32>) -> !d_tensor.tensor<[%m2, %n], f32>
  %out = "d_tensor.cast"(%prod)
    : (!d_tensor.tensor<[%m2, %n], f32>) -> !d_tensor.tensor<[%m2, %n], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%m2, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %3 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// VERIFY:   %4 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// VERIFY:   %5 = "d_tensor.nat.add"(%0, %3) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %6 = "d_tensor.nat.mul"(%5, %4) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %7 = "d_tensor.nat.add"(%6, %3) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %8 = "d_tensor.nat.add"(%1, %3) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %9 = "d_tensor.nat.mul"(%8, %4) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %10 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%7, %9], f32>
// VERIFY:   %11 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%9, %2], f32>
// VERIFY:   %12 = "d_tensor.matmul"(%10, %11) : (!d_tensor.tensor<[%7, %9], f32>, !d_tensor.tensor<[%9, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %13 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %14 = "d_tensor.add"(%12, %13) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %15 = "d_tensor.mul"(%14, %14) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %16 = "d_tensor.cast"(%15) : (!d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   "test.keep"(%16) : (!d_tensor.tensor<[%7, %2], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %3 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CANON:   %4 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CANON:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CANON:   %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1, %2], f32>
// CANON:   %7 = "d_tensor.matmul"(%5, %6) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %8 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %9 = "d_tensor.add"(%7, %8) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %10 = "d_tensor.mul"(%9, %9) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %11 = "d_tensor.cast"(%10) : (!d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   "test.keep"(%11) : (!d_tensor.tensor<[%0, %2], f32>) -> ()
// CANON: }

// PIPE1: builtin.module {
// PIPE1:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE1:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE1:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE1:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// PIPE1:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1, %2], f32>
// PIPE1:   %5 = "d_tensor.matmul"(%3, %4) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE1:   %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %2], f32>
// PIPE1:   %7 = "d_tensor.add"(%5, %6) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE1:   %8 = "d_tensor.mul"(%7, %7) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE1:   %9 = "d_tensor.cast"(%8) : (!d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE1:   "test.keep"(%9) : (!d_tensor.tensor<[%0, %2], f32>) -> ()
// PIPE1: }

// PIPE2: builtin.module {
// PIPE2:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE2:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE2:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE2:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// PIPE2:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1, %2], f32>
// PIPE2:   %5 = "d_tensor.matmul"(%3, %4) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %7 = "d_tensor.add"(%5, %6) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %8 = "d_tensor.mul"(%7, %7) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %9 = "d_tensor.cast"(%8) : (!d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   "test.keep"(%9) : (!d_tensor.tensor<[%0, %2], f32>) -> ()
// PIPE2: }
