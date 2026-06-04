// Purpose: Ensure shape-canonicalize deep-RAUW updates all type-embedded dim uses across multiple result types.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%x, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %e0 = "dtensor.empty"() : () -> !dtensor.tensor<[%s], f32>
  %e1 = "dtensor.empty"() : () -> !dtensor.tensor<[%s], f32>
  %c1 = "dtensor.cast"(%e1) : (!dtensor.tensor<[%s], f32>) -> !dtensor.tensor<[%s], f32>
  %a = "dtensor.add"(%e0, %c1)
    : (!dtensor.tensor<[%s], f32>, !dtensor.tensor<[%s], f32>) -> !dtensor.tensor<[%s], f32>
  %m = "dtensor.mul"(%a, %e0)
    : (!dtensor.tensor<[%s], f32>, !dtensor.tensor<[%s], f32>) -> !dtensor.tensor<[%s], f32>
  "test.keep"(%m) : (!dtensor.tensor<[%s], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.nat.add"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%2], f32>
// VERIFY:   %4 = "dtensor.empty"() : () -> !dtensor.tensor<[%2], f32>
// VERIFY:   %5 = "dtensor.cast"(%4) : (!dtensor.tensor<[%2], f32>) -> !dtensor.tensor<[%2], f32>
// VERIFY:   %6 = "dtensor.add"(%3, %5) : (!dtensor.tensor<[%2], f32>, !dtensor.tensor<[%2], f32>) -> !dtensor.tensor<[%2], f32>
// VERIFY:   %7 = "dtensor.mul"(%6, %3) : (!dtensor.tensor<[%2], f32>, !dtensor.tensor<[%2], f32>) -> !dtensor.tensor<[%2], f32>
// VERIFY:   "test.keep"(%7) : (!dtensor.tensor<[%2], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %1 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CANON:   %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CANON:   %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CANON:   %4 = "dtensor.cast"(%3) : (!dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// CANON:   %5 = "dtensor.add"(%2, %4) : (!dtensor.tensor<[%0], f32>, !dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// CANON:   %6 = "dtensor.mul"(%5, %2) : (!dtensor.tensor<[%0], f32>, !dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// CANON:   "test.keep"(%6) : (!dtensor.tensor<[%0], f32>) -> ()
// CANON: }

// PIPE: builtin.module {
// PIPE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE:   %1 = "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// PIPE:   %2 = "dtensor.cast"(%1) : (!dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// PIPE:   %3 = "dtensor.add"(%1, %2) : (!dtensor.tensor<[%0], f32>, !dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// PIPE:   %4 = "dtensor.mul"(%3, %1) : (!dtensor.tensor<[%0], f32>, !dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// PIPE:   "test.keep"(%4) : (!dtensor.tensor<[%0], f32>) -> ()
// PIPE: }
