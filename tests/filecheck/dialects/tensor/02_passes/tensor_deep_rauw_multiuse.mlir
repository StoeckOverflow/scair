// Purpose: Ensure shape-canonicalize deep-RAUW updates all type-embedded dim uses across multiple result types.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

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
}

// VERIFY: "dtensor.nat.add"
// VERIFY: !dtensor.tensor<[%2], f32>

// CANON: [[X:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NOT: "dtensor.nat.add"
// CANON: "dtensor.empty"() : () -> !dtensor.tensor<[[[X]]], f32>
// CANON: "dtensor.empty"() : () -> !dtensor.tensor<[[[X]]], f32>
// CANON: "dtensor.cast"(%{{[0-9]+}}) : (!dtensor.tensor<[[[X]]], f32>) -> !dtensor.tensor<[[[X]]], f32>
// CANON: "dtensor.add"(%{{[0-9]+}}, %{{[0-9]+}}) : (!dtensor.tensor<[[[X]]], f32>, !dtensor.tensor<[[[X]]], f32>) -> !dtensor.tensor<[[[X]]], f32>
// CANON: "dtensor.mul"(%{{[0-9]+}}, %{{[0-9]+}}) : (!dtensor.tensor<[[[X]]], f32>, !dtensor.tensor<[[[X]]], f32>) -> !dtensor.tensor<[[[X]]], f32>

// PIPE-NOT: "dtensor.nat.add"
// PIPE: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
