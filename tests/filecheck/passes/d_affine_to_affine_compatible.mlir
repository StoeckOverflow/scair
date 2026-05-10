// RUN: scair-opt %s --allow-unregistered-dialect -p d-affine-to-affine-compatible | filecheck %s

builtin.module {
  func.func @static_bridge(%lb: index, %ub: index, %init: index) -> index {
    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%acc] : (index)[index] -> index
      %cap = d_affine.min affine_map<(d0)[s0] -> (d0 + s0)>(%next)[%ub] : (index)[index] -> index
      d_affine.yield %cap : (index)
    }
    func.return %sum : index
  }
}

// CHECK-LABEL: func.func @static_bridge
// CHECK: %[[SUM:[0-9]+]] = affine.for %[[IV:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 iter_args(%[[ACC:[0-9]+]] = %{{[0-9]+}}) -> (index) {
// CHECK: %[[NEXT:[0-9]+]] = "affine.apply"(%[[IV]], %[[ACC]]) <{map = #map{{[0-9]*}}}> : (index, index) -> index
// CHECK: %[[CAP:[0-9]+]] = "affine.min"(%[[NEXT]], %{{[0-9]+}}) <{map = #map{{[0-9]*}}}> : (index, index) -> index
// CHECK: affine.yield %[[CAP]] : index
// CHECK: func.return %[[SUM]] : index
// CHECK-NOT: d_affine
