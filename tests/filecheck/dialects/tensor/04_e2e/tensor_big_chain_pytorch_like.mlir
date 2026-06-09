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
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %h = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %o = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat

  %k_norm = "d_tensor.nat.add"(%k, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %m_norm = "d_tensor.nat.mul"(%m, %o) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  %x = "test.x"() : () -> !d_tensor.tensor<[%m_norm, %k_norm], f32>
  %w1 = "test.w1"() : () -> !d_tensor.tensor<[%k_norm, %h], f32>
  %b1 = "test.b1"() : () -> !d_tensor.tensor<[%m_norm, %h], f32>
  %w2 = "test.w2"() : () -> !d_tensor.tensor<[%h, %n], f32>
  %b2 = "test.b2"() : () -> !d_tensor.tensor<[%m_norm, %n], f32>

  %l1 = "d_tensor.matmul"(%x, %w1)
    : (!d_tensor.tensor<[%m_norm, %k_norm], f32>, !d_tensor.tensor<[%k_norm, %h], f32>) -> !d_tensor.tensor<[%m_norm, %h], f32>
  %a1 = "d_tensor.add"(%l1, %b1)
    : (!d_tensor.tensor<[%m_norm, %h], f32>, !d_tensor.tensor<[%m_norm, %h], f32>) -> !d_tensor.tensor<[%m_norm, %h], f32>
  %m1 = "d_tensor.mul"(%a1, %a1)
    : (!d_tensor.tensor<[%m_norm, %h], f32>, !d_tensor.tensor<[%m_norm, %h], f32>) -> !d_tensor.tensor<[%m_norm, %h], f32>

  %l2 = "d_tensor.matmul"(%m1, %w2)
    : (!d_tensor.tensor<[%m_norm, %h], f32>, !d_tensor.tensor<[%h, %n], f32>) -> !d_tensor.tensor<[%m_norm, %n], f32>
  %a2 = "d_tensor.add"(%l2, %b2)
    : (!d_tensor.tensor<[%m_norm, %n], f32>, !d_tensor.tensor<[%m_norm, %n], f32>) -> !d_tensor.tensor<[%m_norm, %n], f32>
  %out = "d_tensor.mul"(%a2, %a2)
    : (!d_tensor.tensor<[%m_norm, %n], f32>, !d_tensor.tensor<[%m_norm, %n], f32>) -> !d_tensor.tensor<[%m_norm, %n], f32>

  "test.keep_big_like"(%out) : (!d_tensor.tensor<[%m_norm, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %3 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %4 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// VERIFY:   %5 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// VERIFY:   %6 = "d_tensor.nat.add"(%1, %4) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %7 = "d_tensor.nat.mul"(%0, %5) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %8 = "test.x"() : () -> !d_tensor.tensor<[%7, %6], f32>
// VERIFY:   %9 = "test.w1"() : () -> !d_tensor.tensor<[%6, %3], f32>
// VERIFY:   %10 = "test.b1"() : () -> !d_tensor.tensor<[%7, %3], f32>
// VERIFY:   %11 = "test.w2"() : () -> !d_tensor.tensor<[%3, %2], f32>
// VERIFY:   %12 = "test.b2"() : () -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %13 = "d_tensor.matmul"(%8, %9) : (!d_tensor.tensor<[%7, %6], f32>, !d_tensor.tensor<[%6, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// VERIFY:   %14 = "d_tensor.add"(%13, %10) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%7, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// VERIFY:   %15 = "d_tensor.mul"(%14, %14) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%7, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// VERIFY:   %16 = "d_tensor.matmul"(%15, %11) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%3, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %17 = "d_tensor.add"(%16, %12) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   %18 = "d_tensor.mul"(%17, %17) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// VERIFY:   "test.keep_big_like"(%18) : (!d_tensor.tensor<[%7, %2], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %3 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %4 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CANON:   %5 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CANON:   %6 = "test.x"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CANON:   %7 = "test.w1"() : () -> !d_tensor.tensor<[%1, %3], f32>
// CANON:   %8 = "test.b1"() : () -> !d_tensor.tensor<[%0, %3], f32>
// CANON:   %9 = "test.w2"() : () -> !d_tensor.tensor<[%3, %2], f32>
// CANON:   %10 = "test.b2"() : () -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %11 = "d_tensor.matmul"(%6, %7) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// CANON:   %12 = "d_tensor.add"(%11, %8) : (!d_tensor.tensor<[%0, %3], f32>, !d_tensor.tensor<[%0, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// CANON:   %13 = "d_tensor.mul"(%12, %12) : (!d_tensor.tensor<[%0, %3], f32>, !d_tensor.tensor<[%0, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// CANON:   %14 = "d_tensor.matmul"(%13, %9) : (!d_tensor.tensor<[%0, %3], f32>, !d_tensor.tensor<[%3, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %15 = "d_tensor.add"(%14, %10) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   %16 = "d_tensor.mul"(%15, %15) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// CANON:   "test.keep_big_like"(%16) : (!d_tensor.tensor<[%0, %2], f32>) -> ()
// CANON: }

// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %3 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %4 = "test.x"() : () -> !d_tensor.tensor<[%0, %1], f32>
// PIPE:   %5 = "test.w1"() : () -> !d_tensor.tensor<[%1, %3], f32>
// PIPE:   %6 = "test.b1"() : () -> !d_tensor.tensor<[%0, %3], f32>
// PIPE:   %7 = "test.w2"() : () -> !d_tensor.tensor<[%3, %2], f32>
// PIPE:   %8 = "test.b2"() : () -> !d_tensor.tensor<[%0, %2], f32>
// PIPE:   %9 = "d_tensor.matmul"(%4, %5) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// PIPE:   %10 = "d_tensor.add"(%9, %6) : (!d_tensor.tensor<[%0, %3], f32>, !d_tensor.tensor<[%0, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// PIPE:   %11 = "d_tensor.mul"(%10, %10) : (!d_tensor.tensor<[%0, %3], f32>, !d_tensor.tensor<[%0, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// PIPE:   %12 = "d_tensor.matmul"(%11, %7) : (!d_tensor.tensor<[%0, %3], f32>, !d_tensor.tensor<[%3, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE:   %13 = "d_tensor.add"(%12, %8) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE:   %14 = "d_tensor.mul"(%13, %13) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE:   "test.keep_big_like"(%14) : (!d_tensor.tensor<[%0, %2], f32>) -> ()
// PIPE: }

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %3 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %4 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CSE:   %5 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CSE:   %6 = "d_tensor.nat.add"(%1, %4) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CSE:   %7 = "d_tensor.nat.mul"(%0, %5) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CSE:   %8 = "test.x"() : () -> !d_tensor.tensor<[%7, %6], f32>
// CSE:   %9 = "test.w1"() : () -> !d_tensor.tensor<[%6, %3], f32>
// CSE:   %10 = "test.b1"() : () -> !d_tensor.tensor<[%7, %3], f32>
// CSE:   %11 = "test.w2"() : () -> !d_tensor.tensor<[%3, %2], f32>
// CSE:   %12 = "test.b2"() : () -> !d_tensor.tensor<[%7, %2], f32>
// CSE:   %13 = "d_tensor.matmul"(%8, %9) : (!d_tensor.tensor<[%7, %6], f32>, !d_tensor.tensor<[%6, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// CSE:   %14 = "d_tensor.add"(%13, %10) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%7, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// CSE:   %15 = "d_tensor.mul"(%14, %14) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%7, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// CSE:   %16 = "d_tensor.matmul"(%15, %11) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%3, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CSE:   %17 = "d_tensor.add"(%16, %12) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CSE:   %18 = "d_tensor.mul"(%17, %17) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CSE:   "test.keep_big_like"(%18) : (!d_tensor.tensor<[%7, %2], f32>) -> ()
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %3 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %4 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// DCE:   %5 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// DCE:   %6 = "d_tensor.nat.add"(%1, %4) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// DCE:   %7 = "d_tensor.nat.mul"(%0, %5) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// DCE:   %8 = "test.x"() : () -> !d_tensor.tensor<[%7, %6], f32>
// DCE:   %9 = "test.w1"() : () -> !d_tensor.tensor<[%6, %3], f32>
// DCE:   %10 = "test.b1"() : () -> !d_tensor.tensor<[%7, %3], f32>
// DCE:   %11 = "test.w2"() : () -> !d_tensor.tensor<[%3, %2], f32>
// DCE:   %12 = "test.b2"() : () -> !d_tensor.tensor<[%7, %2], f32>
// DCE:   %13 = "d_tensor.matmul"(%8, %9) : (!d_tensor.tensor<[%7, %6], f32>, !d_tensor.tensor<[%6, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// DCE:   %14 = "d_tensor.add"(%13, %10) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%7, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// DCE:   %15 = "d_tensor.mul"(%14, %14) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%7, %3], f32>) -> !d_tensor.tensor<[%7, %3], f32>
// DCE:   %16 = "d_tensor.matmul"(%15, %11) : (!d_tensor.tensor<[%7, %3], f32>, !d_tensor.tensor<[%3, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// DCE:   %17 = "d_tensor.add"(%16, %12) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// DCE:   %18 = "d_tensor.mul"(%17, %17) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// DCE:   "test.keep_big_like"(%18) : (!d_tensor.tensor<[%7, %2], f32>) -> ()
// DCE: }
