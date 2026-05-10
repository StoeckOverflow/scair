// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-nats-from-asserts | filecheck %s

builtin.module {
  func.func @refine_asserted_positive_factor(%k0: !dtensor.nat, %k1: !dtensor.nat) {
    %k1_idx = "dtensor.shape.to_index"(%k1) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    "test.keep"(%ub) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @refine_zero_less_than_factor(%k: !dtensor.nat) {
    %idx = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%c0, %idx) <{predicate = 2 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k must be positive"}> : (i1) -> ()
    %again = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    "test.keep"(%again) : (index) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @refine_asserted_positive_factor
// CHECK-SAME: %{{[0-9]+}}: !dtensor.nat, %[[K1:[0-9]+]]: !dtensor.nat
// CHECK: %[[OK:[0-9]+]] = "arith.cmpi"
// CHECK: "cf.assert"(%[[OK]]) <{msg = "k1 must be positive"}> {scair.refine_positive_nats_from_asserts.done = "true"}
// CHECK: %[[K1_POS:[0-9]+]] = "dtensor.nat.refine_positive"(%[[K1]], %[[OK]]) : (!dtensor.nat, i1) -> !dtensor.posnat
// CHECK: "dtensor.nat.mul"(%{{[0-9]+}}, %[[K1_POS]]) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat

// CHECK-LABEL: func.func @refine_zero_less_than_factor
// CHECK-SAME: %[[K:[0-9]+]]: !dtensor.nat
// CHECK: %[[OK2:[0-9]+]] = "arith.cmpi"
// CHECK: "cf.assert"(%[[OK2]]) <{msg = "k must be positive"}> {scair.refine_positive_nats_from_asserts.done = "true"}
// CHECK: %[[K_POS:[0-9]+]] = "dtensor.nat.refine_positive"(%[[K]], %[[OK2]]) : (!dtensor.nat, i1) -> !dtensor.posnat
// CHECK: "dtensor.shape.to_index"(%[[K_POS]]) : (!dtensor.posnat) -> index
