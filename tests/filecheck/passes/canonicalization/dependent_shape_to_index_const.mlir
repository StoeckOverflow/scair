// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %n = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
  %idx = "d_tensor.shape.to_index"(%n) : (!d_tensor.nat) -> index
  "test.keep"(%idx) : (index) -> ()
}

// CHECK: "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK-NOT: "d_tensor.shape.to_index"
