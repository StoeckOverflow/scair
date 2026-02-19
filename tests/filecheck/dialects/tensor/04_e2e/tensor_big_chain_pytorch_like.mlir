// Purpose: PyTorch-like symbolic-shape chain stressing deep RAUW after shape-canonicalize and full-pipeline safety.
// Coverage checklist (existing -> gap -> this file):
// - Existing big-chain tests cover representative paths -> add focused MLP-like chain with explicit nat.add(x,0)/nat.mul(x,1) folding + type-dim rewrite checks.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %h = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat

  %k_norm = "dtensor.nat.add"(%k, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m_norm = "dtensor.nat.mul"(%m, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %x = "test.x"() : () -> !dtensor.tensor<[%m_norm, %k_norm], f32>
  %w1 = "test.w1"() : () -> !dtensor.tensor<[%k_norm, %h], f32>
  %b1 = "test.b1"() : () -> !dtensor.tensor<[%m_norm, %h], f32>
  %w2 = "test.w2"() : () -> !dtensor.tensor<[%h, %n], f32>
  %b2 = "test.b2"() : () -> !dtensor.tensor<[%m_norm, %n], f32>

  %l1 = "dtensor.matmul"(%x, %w1)
    : (!dtensor.tensor<[%m_norm, %k_norm], f32>, !dtensor.tensor<[%k_norm, %h], f32>) -> !dtensor.tensor<[%m_norm, %h], f32>
  %a1 = "dtensor.add"(%l1, %b1)
    : (!dtensor.tensor<[%m_norm, %h], f32>, !dtensor.tensor<[%m_norm, %h], f32>) -> !dtensor.tensor<[%m_norm, %h], f32>
  %m1 = "dtensor.mul"(%a1, %a1)
    : (!dtensor.tensor<[%m_norm, %h], f32>, !dtensor.tensor<[%m_norm, %h], f32>) -> !dtensor.tensor<[%m_norm, %h], f32>

  %l2 = "dtensor.matmul"(%m1, %w2)
    : (!dtensor.tensor<[%m_norm, %h], f32>, !dtensor.tensor<[%h, %n], f32>) -> !dtensor.tensor<[%m_norm, %n], f32>
  %a2 = "dtensor.add"(%l2, %b2)
    : (!dtensor.tensor<[%m_norm, %n], f32>, !dtensor.tensor<[%m_norm, %n], f32>) -> !dtensor.tensor<[%m_norm, %n], f32>
  %out = "dtensor.mul"(%a2, %a2)
    : (!dtensor.tensor<[%m_norm, %n], f32>, !dtensor.tensor<[%m_norm, %n], f32>) -> !dtensor.tensor<[%m_norm, %n], f32>

  "test.keep_big_like"(%out) : (!dtensor.tensor<[%m_norm, %n], f32>) -> ()
}

// VERIFY: "dtensor.matmul"
// VERIFY: "dtensor.add"
// VERIFY: "dtensor.mul"
// VERIFY: "test.keep_big_like"

// CANON-NOT: "dtensor.nat.add"
// CANON-NOT: "dtensor.nat.mul"
// CANON: "test.keep_big_like"

// PIPE-NOT: "dtensor.nat.add"
// PIPE-NOT: "dtensor.nat.mul"
// PIPE: "dtensor.matmul"
// PIPE: "dtensor.matmul"
// PIPE: "test.keep_big_like"(%[[OUT:[0-9]+]]) : (!dtensor.tensor<[%0, %2], f32>) -> ()

// CSE: "test.keep_big_like"
// DCE: "test.keep_big_like"
