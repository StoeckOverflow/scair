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
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index

  %m0 = "arith.addi"(%m, %z) : (index, index) -> index
  %m1 = "arith.muli"(%m0, %o) : (index, index) -> index
  %m2 = "arith.addi"(%m1, %z) : (index, index) -> index

  %k0 = "arith.addi"(%k, %z) : (index, index) -> index
  %k1 = "arith.muli"(%k0, %o) : (index, index) -> index

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
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.index"() : () -> index
// VERIFY:   %2 = "test.index"() : () -> index
// VERIFY:   %3 = "arith.constant"() <{value = 0 : index}> : () -> index
// VERIFY:   %4 = "arith.constant"() <{value = 1 : index}> : () -> index
// VERIFY:   %5 = "arith.addi"(%0, %3) {{.*}} : (index, index) -> index
// VERIFY:   %6 = "arith.muli"(%5, %4) {{.*}} : (index, index) -> index
// VERIFY:   %7 = "arith.addi"(%6, %3) {{.*}} : (index, index) -> index
// VERIFY:   %8 = "arith.addi"(%1, %3) {{.*}} : (index, index) -> index
// VERIFY:   %9 = "arith.muli"(%8, %4) {{.*}} : (index, index) -> index
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
// CANON:   %0 = "test.index"() : () -> index
// CANON:   %1 = "test.index"() : () -> index
// CANON:   %2 = "test.index"() : () -> index
// CANON:   %3 = "arith.constant"() <{value = 0 : index}> : () -> index
// CANON:   %4 = "arith.constant"() <{value = 1 : index}> : () -> index
// CANON:   %5 = "arith.addi"(%0, %3) {{.*}} : (index, index) -> index
// CANON:   %6 = "arith.muli"(%5, %4) {{.*}} : (index, index) -> index
// CANON:   %7 = "arith.addi"(%6, %3) {{.*}} : (index, index) -> index
// CANON:   %8 = "arith.addi"(%1, %3) {{.*}} : (index, index) -> index
// CANON:   %9 = "arith.muli"(%8, %4) {{.*}} : (index, index) -> index
// CANON:   %10 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%7, %9], f32>
// CANON:   %11 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%9, %2], f32>
// CANON:   %12 = "d_tensor.matmul"(%10, %11) : (!d_tensor.tensor<[%7, %9], f32>, !d_tensor.tensor<[%9, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CANON:   %13 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%7, %2], f32>
// CANON:   %14 = "d_tensor.add"(%12, %13) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CANON:   %15 = "d_tensor.mul"(%14, %14) : (!d_tensor.tensor<[%7, %2], f32>, !d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CANON:   %16 = "d_tensor.cast"(%15) : (!d_tensor.tensor<[%7, %2], f32>) -> !d_tensor.tensor<[%7, %2], f32>
// CANON:   "test.keep"(%16) : (!d_tensor.tensor<[%7, %2], f32>) -> ()
// CANON: }

// PIPE1: builtin.module {
// PIPE1:   %0 = "test.index"() : () -> index
// PIPE1:   %1 = "test.index"() : () -> index
// PIPE1:   %2 = "test.index"() : () -> index
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
// PIPE2:   %0 = "test.index"() : () -> index
// PIPE2:   %1 = "test.index"() : () -> index
// PIPE2:   %2 = "test.index"() : () -> index
// PIPE2:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// PIPE2:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1, %2], f32>
// PIPE2:   %5 = "d_tensor.matmul"(%3, %4) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %7 = "d_tensor.add"(%5, %6) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %8 = "d_tensor.mul"(%7, %7) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   %9 = "d_tensor.cast"(%8) : (!d_tensor.tensor<[%0, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// PIPE2:   "test.keep"(%9) : (!d_tensor.tensor<[%0, %2], f32>) -> ()
// PIPE2: }
