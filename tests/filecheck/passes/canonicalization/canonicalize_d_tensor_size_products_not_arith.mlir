// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-size-products | filecheck %s --implicit-check-not=d_tensor.size.mul

builtin.module {
  func.func @ordinary_arith_is_not_product_proof(%lhs: index, %rhs: index) -> index {
    %prod = "arith.muli"(%lhs, %rhs) : (index, index) -> index
    "func.return"(%prod) : (index) -> ()
  }
}

// CHECK-LABEL: func.func @ordinary_arith_is_not_product_proof
// CHECK: %[[PROD:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %{{[0-9]+}}) {{.*}} : (index, index) -> index
// CHECK: func.return %[[PROD]] : index
