// Purpose: PyTorch-like symbolic-shape chain stressing deep RAUW after shape-canonicalize and full-pipeline safety.
// - Existing big-chain tests cover representative paths -> add focused MLP-like chain with explicit nat.add(x,0)/nat.mul(x,1) folding + type-dim rewrite checks.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

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

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %4 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// VERIFY:   %5 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// VERIFY:   %6 = "dtensor.nat.add"(%1, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %7 = "dtensor.nat.mul"(%0, %5) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %8 = "test.x"() : () -> !dtensor.tensor<[%7, %6], f32>
// VERIFY:   %9 = "test.w1"() : () -> !dtensor.tensor<[%6, %3], f32>
// VERIFY:   %10 = "test.b1"() : () -> !dtensor.tensor<[%7, %3], f32>
// VERIFY:   %11 = "test.w2"() : () -> !dtensor.tensor<[%3, %2], f32>
// VERIFY:   %12 = "test.b2"() : () -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %13 = "dtensor.matmul"(%8, %9) : (!dtensor.tensor<[%7, %6], f32>, !dtensor.tensor<[%6, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// VERIFY:   %14 = "dtensor.add"(%13, %10) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%7, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// VERIFY:   %15 = "dtensor.mul"(%14, %14) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%7, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// VERIFY:   %16 = "dtensor.matmul"(%15, %11) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %17 = "dtensor.add"(%16, %12) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   %18 = "dtensor.mul"(%17, %17) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// VERIFY:   "test.keep_big_like"(%18) : (!dtensor.tensor<[%7, %2], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %4 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CANON:   %5 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CANON:   %6 = "test.x"() : () -> !dtensor.tensor<[%0, %1], f32>
// CANON:   %7 = "test.w1"() : () -> !dtensor.tensor<[%1, %3], f32>
// CANON:   %8 = "test.b1"() : () -> !dtensor.tensor<[%0, %3], f32>
// CANON:   %9 = "test.w2"() : () -> !dtensor.tensor<[%3, %2], f32>
// CANON:   %10 = "test.b2"() : () -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %11 = "dtensor.matmul"(%6, %7) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%1, %3], f32>) -> !dtensor.tensor<[%0, %3], f32>
// CANON:   %12 = "dtensor.add"(%11, %8) : (!dtensor.tensor<[%0, %3], f32>, !dtensor.tensor<[%0, %3], f32>) -> !dtensor.tensor<[%0, %3], f32>
// CANON:   %13 = "dtensor.mul"(%12, %12) : (!dtensor.tensor<[%0, %3], f32>, !dtensor.tensor<[%0, %3], f32>) -> !dtensor.tensor<[%0, %3], f32>
// CANON:   %14 = "dtensor.matmul"(%13, %9) : (!dtensor.tensor<[%0, %3], f32>, !dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %15 = "dtensor.add"(%14, %10) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   %16 = "dtensor.mul"(%15, %15) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// CANON:   "test.keep_big_like"(%16) : (!dtensor.tensor<[%0, %2], f32>) -> ()
// CANON: }

// PIPE: builtin.module {
// PIPE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE:   %4 = "test.x"() : () -> !dtensor.tensor<[%0, %1], f32>
// PIPE:   %5 = "test.w1"() : () -> !dtensor.tensor<[%1, %3], f32>
// PIPE:   %6 = "test.b1"() : () -> !dtensor.tensor<[%0, %3], f32>
// PIPE:   %7 = "test.w2"() : () -> !dtensor.tensor<[%3, %2], f32>
// PIPE:   %8 = "test.b2"() : () -> !dtensor.tensor<[%0, %2], f32>
// PIPE:   %9 = "dtensor.matmul"(%4, %5) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%1, %3], f32>) -> !dtensor.tensor<[%0, %3], f32>
// PIPE:   %10 = "dtensor.add"(%9, %6) : (!dtensor.tensor<[%0, %3], f32>, !dtensor.tensor<[%0, %3], f32>) -> !dtensor.tensor<[%0, %3], f32>
// PIPE:   %11 = "dtensor.mul"(%10, %10) : (!dtensor.tensor<[%0, %3], f32>, !dtensor.tensor<[%0, %3], f32>) -> !dtensor.tensor<[%0, %3], f32>
// PIPE:   %12 = "dtensor.matmul"(%11, %7) : (!dtensor.tensor<[%0, %3], f32>, !dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE:   %13 = "dtensor.add"(%12, %8) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE:   %14 = "dtensor.mul"(%13, %13) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%0, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// PIPE:   "test.keep_big_like"(%14) : (!dtensor.tensor<[%0, %2], f32>) -> ()
// PIPE: }

// CSE: builtin.module {
// CSE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE:   %4 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CSE:   %5 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CSE:   %6 = "dtensor.nat.add"(%1, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CSE:   %7 = "dtensor.nat.mul"(%0, %5) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CSE:   %8 = "test.x"() : () -> !dtensor.tensor<[%7, %6], f32>
// CSE:   %9 = "test.w1"() : () -> !dtensor.tensor<[%6, %3], f32>
// CSE:   %10 = "test.b1"() : () -> !dtensor.tensor<[%7, %3], f32>
// CSE:   %11 = "test.w2"() : () -> !dtensor.tensor<[%3, %2], f32>
// CSE:   %12 = "test.b2"() : () -> !dtensor.tensor<[%7, %2], f32>
// CSE:   %13 = "dtensor.matmul"(%8, %9) : (!dtensor.tensor<[%7, %6], f32>, !dtensor.tensor<[%6, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// CSE:   %14 = "dtensor.add"(%13, %10) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%7, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// CSE:   %15 = "dtensor.mul"(%14, %14) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%7, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// CSE:   %16 = "dtensor.matmul"(%15, %11) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// CSE:   %17 = "dtensor.add"(%16, %12) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// CSE:   %18 = "dtensor.mul"(%17, %17) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// CSE:   "test.keep_big_like"(%18) : (!dtensor.tensor<[%7, %2], f32>) -> ()
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE:   %4 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// DCE:   %5 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// DCE:   %6 = "dtensor.nat.add"(%1, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// DCE:   %7 = "dtensor.nat.mul"(%0, %5) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// DCE:   %8 = "test.x"() : () -> !dtensor.tensor<[%7, %6], f32>
// DCE:   %9 = "test.w1"() : () -> !dtensor.tensor<[%6, %3], f32>
// DCE:   %10 = "test.b1"() : () -> !dtensor.tensor<[%7, %3], f32>
// DCE:   %11 = "test.w2"() : () -> !dtensor.tensor<[%3, %2], f32>
// DCE:   %12 = "test.b2"() : () -> !dtensor.tensor<[%7, %2], f32>
// DCE:   %13 = "dtensor.matmul"(%8, %9) : (!dtensor.tensor<[%7, %6], f32>, !dtensor.tensor<[%6, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// DCE:   %14 = "dtensor.add"(%13, %10) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%7, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// DCE:   %15 = "dtensor.mul"(%14, %14) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%7, %3], f32>) -> !dtensor.tensor<[%7, %3], f32>
// DCE:   %16 = "dtensor.matmul"(%15, %11) : (!dtensor.tensor<[%7, %3], f32>, !dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// DCE:   %17 = "dtensor.add"(%16, %12) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// DCE:   %18 = "dtensor.mul"(%17, %17) : (!dtensor.tensor<[%7, %2], f32>, !dtensor.tensor<[%7, %2], f32>) -> !dtensor.tensor<[%7, %2], f32>
// DCE:   "test.keep_big_like"(%18) : (!dtensor.tensor<[%7, %2], f32>) -> ()
// DCE: }
