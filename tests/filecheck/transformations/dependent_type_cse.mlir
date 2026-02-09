// RUN: scair-opt %s --allow-unregistered-dialect --skip-verify --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s

builtin.module {
  %t0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %t1 = "arith.constant"() <{value = 0 : index}> : () -> index
  "test.use"() {dep = !tensor.vector<%t1, f32>} : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NOT: "arith.constant"
// CHECK: "test.use"() {dep = !tensor.vector<%0, f32>} : () -> ()
// CHECK: }

// -----

builtin.module {
  %t0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %a = "arith.constant"() <{value = 1 : i32}> : () -> !tensor.vector<%t0, f32>
  %t1 = "arith.constant"() <{value = 1 : index}> : () -> index
  %b = "arith.constant"() <{value = 1 : i32}> : () -> !tensor.vector<%t1, f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %1 = "arith.constant"() <{value = 1 : i32}> : () -> !tensor.vector<%0, f32>
// CHECK: %2 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK: %3 = "arith.constant"() <{value = 1 : i32}> : () -> !tensor.vector<%2, f32>
// CHECK: }
