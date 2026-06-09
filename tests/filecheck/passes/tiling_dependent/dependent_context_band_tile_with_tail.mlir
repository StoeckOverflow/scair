// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-context-band-tile-with-tail:4 | filecheck %s

builtin.module {
  %m = "test.arg"() : () -> index
  %n = "test.arg"() : () -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index

  d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
    d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      "test.touch"(%i, %j) : (index, index) -> ()
      d_affine.yield
    }
    d_affine.yield
  }
}

// CHECK: d_affine.for %[[I_TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 4 : index
// CHECK: %[[I_CLAMP:[0-9]+]] = d_affine.min
// CHECK: d_affine.for %[[I:[0-9]+]] = #map(%[[I_TILE]]) to #map(%[[I_CLAMP]]) step 1 : index
// CHECK: d_affine.for %[[J_TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 4 : index
// CHECK: %[[J_CLAMP:[0-9]+]] = d_affine.min
// CHECK: d_affine.for %[[J:[0-9]+]] = #map(%[[J_TILE]]) to #map(%[[J_CLAMP]]) step 1 : index
// CHECK: "test.touch"(%[[I]], %[[J]])
