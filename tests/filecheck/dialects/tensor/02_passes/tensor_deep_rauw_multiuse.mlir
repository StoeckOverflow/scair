// Purpose: Ensure shape-canonicalize deep-RAUW updates all type-embedded dim uses across multiple result types.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

builtin.module {
  %x = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %s = "arith.addi"(%x, %z) : (index, index) -> index

  %e0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%s], f32>
  %e1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%s], f32>
  %c1 = "d_tensor.cast"(%e1) : (!d_tensor.tensor<[%s], f32>) -> !d_tensor.tensor<[%s], f32>
  %a = "d_tensor.add"(%e0, %c1)
    : (!d_tensor.tensor<[%s], f32>, !d_tensor.tensor<[%s], f32>) -> !d_tensor.tensor<[%s], f32>
  %m = "d_tensor.mul"(%a, %e0)
    : (!d_tensor.tensor<[%s], f32>, !d_tensor.tensor<[%s], f32>) -> !d_tensor.tensor<[%s], f32>
  "test.keep"(%m) : (!d_tensor.tensor<[%s], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "arith.constant"() <{value = 0 : index}> : () -> index
// VERIFY:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// VERIFY:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %5 = "d_tensor.cast"(%4) : (!d_tensor.tensor<[%2], f32>) -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %6 = "d_tensor.add"(%3, %5) : (!d_tensor.tensor<[%2], f32>, !d_tensor.tensor<[%2], f32>) -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %7 = "d_tensor.mul"(%6, %3) : (!d_tensor.tensor<[%2], f32>, !d_tensor.tensor<[%2], f32>) -> !d_tensor.tensor<[%2], f32>
// VERIFY:   "test.keep"(%7) : (!d_tensor.tensor<[%2], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "test.index"() : () -> index
// CANON:   %1 = "arith.constant"() <{value = 0 : index}> : () -> index
// CANON:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CANON:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2], f32>
// CANON:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2], f32>
// CANON:   %5 = "d_tensor.cast"(%4) : (!d_tensor.tensor<[%2], f32>) -> !d_tensor.tensor<[%2], f32>
// CANON:   %6 = "d_tensor.add"(%3, %5) : (!d_tensor.tensor<[%2], f32>, !d_tensor.tensor<[%2], f32>) -> !d_tensor.tensor<[%2], f32>
// CANON:   %7 = "d_tensor.mul"(%6, %3) : (!d_tensor.tensor<[%2], f32>, !d_tensor.tensor<[%2], f32>) -> !d_tensor.tensor<[%2], f32>
// CANON:   "test.keep"(%7) : (!d_tensor.tensor<[%2], f32>) -> ()
// CANON: }

// PIPE: builtin.module {
// PIPE:   %0 = "test.index"() : () -> index
// PIPE:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   %2 = "d_tensor.cast"(%1) : (!d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// PIPE:   %3 = "d_tensor.add"(%1, %2) : (!d_tensor.tensor<[%0], f32>, !d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// PIPE:   %4 = "d_tensor.mul"(%3, %1) : (!d_tensor.tensor<[%0], f32>, !d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// PIPE:   "test.keep"(%4) : (!d_tensor.tensor<[%0], f32>) -> ()
// PIPE: }
