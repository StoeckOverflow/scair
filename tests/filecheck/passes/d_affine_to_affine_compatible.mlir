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

  func.func @multi_result_bridge(%lb: index, %ub: index, %a: index, %b: index) -> (index, index) {
    %r0, %r1 = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%x = %a : index, %y = %b : index) {
      %next_x = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%x] : (index)[index] -> index
      %next_y = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%next_x)[%y] : (index)[index] -> index
      d_affine.yield %next_x, %next_y : (index, index)
    }
    func.return %r0, %r1 : index, index
  }
}

// CHECK-LABEL: func.func @static_bridge
// CHECK: %[[SUM:[0-9]+]] = affine.for %[[IV:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 iter_args(%[[ACC:[0-9]+]] = %{{[0-9]+}}) -> (index) {
// CHECK: %[[NEXT:[0-9]+]] = "affine.apply"(%[[IV]], %[[ACC]]) <{map = #map{{[0-9]*}}}> : (index, index) -> index
// CHECK: %[[CAP:[0-9]+]] = "affine.min"(%[[NEXT]], %{{[0-9]+}}) <{map = #map{{[0-9]*}}}> : (index, index) -> index
// CHECK: affine.yield %[[CAP]] : index
// CHECK: func.return %[[SUM]] : index
// CHECK-LABEL: func.func @multi_result_bridge
// CHECK: %[[R0:[0-9]+]], %[[R1:[0-9]+]] = affine.for %[[IV:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 iter_args(%[[X:[0-9]+]] = %{{[0-9]+}}, %[[Y:[0-9]+]] = %{{[0-9]+}}) -> (index, index) {
// CHECK: %[[NX:[0-9]+]] = "affine.apply"(%[[IV]], %[[X]]) <{map = #map{{[0-9]*}}}> : (index, index) -> index
// CHECK: %[[NY:[0-9]+]] = "affine.apply"(%[[NX]], %[[Y]]) <{map = #map{{[0-9]*}}}> : (index, index) -> index
// CHECK: affine.yield %[[NX]], %[[NY]] : index, index
// CHECK: func.return %[[R0]], %[[R1]] : index, index
// CHECK-NOT: d_affine
