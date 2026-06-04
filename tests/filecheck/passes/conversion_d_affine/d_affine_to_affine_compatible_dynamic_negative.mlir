// RUN: scair-opt %s --allow-unregistered-dialect -p d-affine-to-affine-compatible | filecheck %s

builtin.module {
  func.func @dynamic_step_stays(%lb: index, %ub: index, %step: index) {
    d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
      d_affine.yield
    }
    func.return
  }
}

// CHECK-LABEL: func.func @dynamic_step_stays
// CHECK-NOT: affine.for
// CHECK: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : index {
// CHECK: d_affine.yield
// CHECK-NOT: affine.for
