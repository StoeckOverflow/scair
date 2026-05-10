// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-affine-context-band-tile-with-tail:4 | filecheck %s

builtin.module {
  %m = "test.arg"() : () -> index
  %n = "test.arg"() : () -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index

  affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
    affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      "test.touch"(%i, %j) : (index, index) -> ()
    }
  }
}

// CHECK: affine.for %[[I_TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 4
// CHECK: affine.for %[[I:[0-9]+]] = #map(%[[I_TILE]]) to min #map{{[0-9]*}}(%[[I_TILE]])[%{{.*}}] step 1
// CHECK: affine.for %[[J_TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 4
// CHECK: affine.for %[[J:[0-9]+]] = #map(%[[J_TILE]]) to min #map{{[0-9]*}}(%[[J_TILE]])[%{{.*}}] step 1
// CHECK: "test.touch"(%[[I]], %[[J]])
