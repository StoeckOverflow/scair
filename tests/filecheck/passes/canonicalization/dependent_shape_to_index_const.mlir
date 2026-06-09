// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %n = "arith.constant"() <{value = 7 : index}> : () -> index
  %t = "test.tensor"() : () -> !d_tensor.tensor<[%n], i32>
  %idx = "d_tensor.dim"(%t) <{axis = 0 : i32}> : (!d_tensor.tensor<[%n], i32>) -> !value<%n>
  "test.keep"(%idx) : (!value<%n>) -> ()
}

// CHECK: %[[N:[0-9]+]] = "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK: "test.keep"(%[[N]]) : (index) -> ()
// CHECK-NOT: "d_tensor.dim"
// CHECK-NOT: d_tensor.shape
