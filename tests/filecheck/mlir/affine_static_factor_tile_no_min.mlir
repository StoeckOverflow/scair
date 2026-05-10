// RUN: mlir-opt %s --affine-loop-tile=tile-size=3 | filecheck %s

#map = affine_map<()[s0] -> (s0 * 3)>
module {
  func.func @static_affine_factor(%k0: index, %A: memref<?xf32>) {
    %cst = arith.constant 0.0 : f32
    affine.for %p = 0 to #map()[%k0] {
      memref.store %cst, %A[%p] : memref<?xf32>
    }
    return
  }
}

// CHECK: affine.for %[[TILE:[A-Za-z0-9_]+]] = 0 to #map()[%{{.*}}] step 3
// CHECK: affine.for %{{.*}} = #map1(%[[TILE]]) to #map2(%[[TILE]])
// CHECK-NOT: to min
// CHECK-NOT: affine.min
