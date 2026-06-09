// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-size-witnesses-from-asserts | filecheck %s

builtin.module {
  func.func @refine_asserted_positive_factor(%k0: !d_tensor.size, %k1_idx: index) {
    %k1 = "d_tensor.size.import"(%k1_idx) : (index) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    "test.keep"(%k) : (!d_tensor.size) -> ()
    "func.return"() : () -> ()
  }

  func.func @refine_zero_less_than_factor(%k_idx: index) {
    %k = "d_tensor.size.import"(%k_idx) : (index) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%c0, %k_idx) <{predicate = 2 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k must be positive"}> : (i1) -> ()
    "test.keep"(%k) : (!d_tensor.size) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @refine_asserted_positive_factor
// CHECK-SAME: %{{[0-9]+}}: !d_tensor.size, %[[K1_IDX:[0-9]+]]: index
// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.import"(%[[K1_IDX]]) : (index) -> !d_tensor.size
// CHECK: %[[OK:[0-9]+]] = "arith.cmpi"
// CHECK: "cf.assert"(%[[OK]]) <{msg = "k1 must be positive"}> {scair.refine_positive_size_witnesses_from_asserts.done = "true"}
// CHECK: %[[PROOF:[0-9]+]] = "d_tensor.size.positive_proof"(%[[K1]], %[[OK]]) : (!d_tensor.size, i1) -> !d_tensor.positive_size_proof
// CHECK: %[[K1_POS:[0-9]+]] = "d_tensor.size.refine_positive"(%[[K1]], %[[PROOF]]) : (!d_tensor.size, !d_tensor.positive_size_proof) -> !d_tensor.pos_size
// CHECK: "d_tensor.size.mul"(%{{[0-9]+}}, %[[K1_POS]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size

// CHECK-LABEL: func.func @refine_zero_less_than_factor
// CHECK-SAME: %[[K_IDX:[0-9]+]]: index
// CHECK: %[[K:[0-9]+]] = "d_tensor.size.import"(%[[K_IDX]]) : (index) -> !d_tensor.size
// CHECK: %[[OK2:[0-9]+]] = "arith.cmpi"
// CHECK: "cf.assert"(%[[OK2]]) <{msg = "k must be positive"}> {scair.refine_positive_size_witnesses_from_asserts.done = "true"}
// CHECK: %[[PROOF2:[0-9]+]] = "d_tensor.size.positive_proof"(%[[K]], %[[OK2]]) : (!d_tensor.size, i1) -> !d_tensor.positive_size_proof
// CHECK: %[[K_POS:[0-9]+]] = "d_tensor.size.refine_positive"(%[[K]], %[[PROOF2]]) : (!d_tensor.size, !d_tensor.positive_size_proof) -> !d_tensor.pos_size
