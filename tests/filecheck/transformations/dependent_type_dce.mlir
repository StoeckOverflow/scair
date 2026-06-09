// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize | filecheck %s

builtin.module {
  %dead = "arith.constant"() <{value = 0 : index}> : () -> index
  %used = "arith.constant"() <{value = 1 : index}> : () -> index
  "test.use"() {dep = !d_tensor.vector<%used, f32>} : () -> ()
}

// CHECK: builtin.module {
// CHECK: %0 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK: "test.use"() {dep = !d_tensor.vector<%0, f32>} : () -> ()
// CHECK: }
