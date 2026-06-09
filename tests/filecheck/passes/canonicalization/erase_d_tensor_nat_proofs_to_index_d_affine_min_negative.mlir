// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | filecheck %s

// d_affine.min can consume direct index sums without bridge casts.
builtin.module {
  func.func @d_affine_min_uses_index_sum(%k0: index, %k1: index) -> index {
    %k = "arith.addi"(%k0, %k1) : (index, index) -> index
    %limit = d_affine.min affine_map<(d0) -> (d0, 64)>(%k)[] : (index)[] -> index
    "func.return"(%limit) : (index) -> ()
  }
}

// CHECK-LABEL: func.func @d_affine_min_uses_index_sum
// CHECK: %[[K:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK: d_affine.min {{.*}}(%[[K]])[] : (index)[] -> index
// CHECK-NOT: d_tensor.
