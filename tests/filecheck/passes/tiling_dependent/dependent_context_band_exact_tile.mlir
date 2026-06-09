// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-context-band-exact-tile | filecheck %s

builtin.module {
  %m0 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %tile_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %m_size = "d_tensor.size.mul"(%m0, %tile_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index

  d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m_size) step 1 : index {
    "test.touch"(%i) : (index) -> ()
    d_affine.yield
  }
}

// CHECK: %[[TILE_NAT:[0-9]+]] = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// CHECK: d_affine.for %[[OUTER:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[TILE_NAT]] : !d_tensor.size
// CHECK: %[[END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[OUTER]])[%[[TILE_NAT]]] : (index)[!d_tensor.size] -> index
// CHECK: d_affine.for %[[INNER:[0-9]+]] = #map(%[[OUTER]]) to #map(%[[END]]) step 1 : index
// CHECK: "test.touch"(%[[INNER]])
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min
