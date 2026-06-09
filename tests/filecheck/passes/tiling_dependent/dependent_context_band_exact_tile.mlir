// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-context-band-exact-tile | filecheck %s

builtin.module {
  %m0 = "arith.constant"() <{value = 3 : index}> : () -> index
  %tile = "arith.constant"() <{value = 4 : index}> : () -> index
  %m = "arith.muli"(%m0, %tile) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index

  d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
    "test.touch"(%i) : (index) -> ()
    d_affine.yield
  }
}

// CHECK: %[[TILE:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: d_affine.for %[[OUTER:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[TILE]] : index
// CHECK: %[[END:[0-9]+]] = "arith.addi"(%[[OUTER]], %[[TILE]])
// CHECK: d_affine.for %[[INNER:[0-9]+]] = #map(%[[OUTER]]) to #map(%[[END]]) step 1 : index
// CHECK: "test.touch"(%[[INNER]])
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min
