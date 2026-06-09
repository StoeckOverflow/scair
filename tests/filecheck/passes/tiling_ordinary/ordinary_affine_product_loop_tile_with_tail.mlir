// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-affine-product-loop-tile-with-tail:3 | filecheck %s

builtin.module {
  %k0 = "test.arg"() : () -> index
  %k1 = "test.arg"() : () -> index
  %k = "arith.muli"(%k0, %k1) : (index, index) -> index
  %out = "test.memref"() : () -> memref<?xf32>
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

  affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
    "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
  }
}

// CHECK: %[[K:[0-9]+]] = "arith.muli"
// CHECK-NOT: d_tensor
// CHECK-NOT: d_affine
// CHECK: affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map{{[0-9]*}}()[%[[K]]] step 3
// CHECK: affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to min #map{{[0-9]*}}(%[[TILE]])[%[[K]]] step 1
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
