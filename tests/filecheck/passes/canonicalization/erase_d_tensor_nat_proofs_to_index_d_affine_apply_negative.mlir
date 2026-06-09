// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | filecheck %s

// d_affine.apply can consume direct index products without bridge casts.
builtin.module {
  func.func @d_affine_apply_uses_index_product(%k0: index) -> index {
    %k1 = "arith.constant"() <{value = 4 : index}> : () -> index
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %shifted = d_affine.apply affine_map<(d0) -> (d0 + 1)>(%k)[] : (index)[] -> index
    "func.return"(%shifted) : (index) -> ()
  }
}

// CHECK-LABEL: func.func @d_affine_apply_uses_index_product
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK: d_affine.apply {{.*}}(%[[K]])[] : (index)[] -> index
// CHECK-NOT: d_tensor.
