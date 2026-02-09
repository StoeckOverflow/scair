// RUN: scair-opt %s --allow-unregistered-dialect --skip-verify -p canonicalize | filecheck %s

builtin.module {
  %dead = "arith.constant"() <{value = 0 : index}> : () -> index
  %used = "arith.constant"() <{value = 1 : index}> : () -> index
  "test.use"() {dep = !tensor.vector<%used, f32>} : () -> ()
}

// CHECK: builtin.module {
// CHECK: %0 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK: "test.use"() {dep = !tensor.vector<%0, f32>} : () -> ()
// CHECK: }
