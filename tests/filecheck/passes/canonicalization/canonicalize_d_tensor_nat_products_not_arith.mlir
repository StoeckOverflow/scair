// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products | filecheck %s --implicit-check-not='!d_tensor.tensor'

builtin.module {
  func.func @ordinary_arith_is_not_product_proof(%lhs: index) -> index {
    %c4 = "arith.constant"() <{value = 4 : index}> : () -> index
    %prod = "arith.muli"(%lhs, %c4) : (index, index) -> index
    "func.return"(%prod) : (index) -> ()
  }
}

// CHECK-LABEL: func.func @ordinary_arith_is_not_product_proof
// CHECK: %[[LHS:[0-9]+]]: index
// CHECK: %[[C4:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[PROD:[0-9]+]] = "arith.muli"(%[[LHS]], %[[C4]]) {{.*}} : (index, index) -> index
// CHECK: func.return %[[PROD]] : index
