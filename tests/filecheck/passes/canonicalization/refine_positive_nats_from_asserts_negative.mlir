// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-nats-from-asserts | filecheck %s

builtin.module {
  func.func @non_strict_nonnegative_is_not_positive(%k: !dtensor.nat) {
    %idx = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%idx, %c0) <{predicate = 5 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k may be zero"}> : (i1) -> ()
    %again = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    "test.keep"(%again) : (index) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @non_strict_nonnegative_is_not_positive
// CHECK-NOT: dtensor.nat.refine_positive
// CHECK: "dtensor.shape.to_index"(%{{[0-9]+}}) : (!dtensor.nat) -> index
