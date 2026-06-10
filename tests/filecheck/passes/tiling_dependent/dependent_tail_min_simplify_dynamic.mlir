// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control | filecheck %s --check-prefix=GUARDED
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s --check-prefix=SIMPLIFIED

builtin.module {
  %k0 = "test.index"() : () -> index
  %k1 = "test.index"() : () -> index
  %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// GUARDED: %[[FULL_BOUND:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %{{[0-9]+}}) {{.*}} : (index, index) -> index
// GUARDED: d_affine.for %[[P:[0-9]+]] = #map(%{{.*}}) to #map(%[[FULL_BOUND]]) step 1 : index iter_args

// SIMPLIFIED: %[[FULL_BOUND:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %{{[0-9]+}}) {{.*}} : (index, index) -> index
// SIMPLIFIED: d_affine.for %[[P:[0-9]+]] = #map(%{{.*}}) to #map(%[[FULL_BOUND]]) step 1 : index iter_args
// SIMPLIFIED-NOT: arith.minsi
