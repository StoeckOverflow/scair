// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-nats-from-asserts | filecheck %s

builtin.module {
  func.func @refine_asserted_positive_factor(%k0: !d_tensor.nat, %k1: !d_tensor.nat) {
    %k1_idx = "d_tensor.shape.to_index"(%k1) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %ub = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    "test.keep"(%ub) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @refine_zero_less_than_factor(%k: !d_tensor.nat) {
    %idx = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%c0, %idx) <{predicate = 2 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k must be positive"}> : (i1) -> ()
    %again = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    "test.keep"(%again) : (index) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @refine_asserted_positive_factor
// CHECK-SAME: %{{[0-9]+}}: !d_tensor.nat, %[[K1:[0-9]+]]: !d_tensor.nat
// CHECK: %[[OK:[0-9]+]] = "arith.cmpi"
// CHECK: "cf.assert"(%[[OK]]) <{msg = "k1 must be positive"}> {scair.refine_positive_nats_from_asserts.done = "true"}
// CHECK: %[[K1_POS:[0-9]+]] = "d_tensor.nat.refine_positive"(%[[K1]], %[[OK]]) : (!d_tensor.nat, i1) -> !d_tensor.posnat
// CHECK: "d_tensor.nat.mul"(%{{[0-9]+}}, %[[K1_POS]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat

// CHECK-LABEL: func.func @refine_zero_less_than_factor
// CHECK-SAME: %[[K:[0-9]+]]: !d_tensor.nat
// CHECK: %[[OK2:[0-9]+]] = "arith.cmpi"
// CHECK: "cf.assert"(%[[OK2]]) <{msg = "k must be positive"}> {scair.refine_positive_nats_from_asserts.done = "true"}
// CHECK: %[[K_POS:[0-9]+]] = "d_tensor.nat.refine_positive"(%[[K]], %[[OK2]]) : (!d_tensor.nat, i1) -> !d_tensor.posnat
// CHECK: "d_tensor.shape.to_index"(%[[K_POS]]) : (!d_tensor.posnat) -> index
