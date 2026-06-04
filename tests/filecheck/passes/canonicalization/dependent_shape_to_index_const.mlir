// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %n = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %idx = "dtensor.shape.to_index"(%n) : (!dtensor.nat) -> index
  "test.keep"(%idx) : (index) -> ()
}

// CHECK: "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK-NOT: "dtensor.shape.to_index"
