// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | filecheck %s

// d_affine loops now consume direct index arithmetic; there is no Nat erasure
// precondition around d_affine.for.
builtin.module {
  func.func @d_affine_for_uses_index_product(%k0: index, %out: memref<?xf32>) {
    %k1 = "arith.constant"() <{value = 4 : index}> : () -> index
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @d_affine_for_uses_index_product
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%[[K]]) step 1
// CHECK-NOT: d_tensor.
