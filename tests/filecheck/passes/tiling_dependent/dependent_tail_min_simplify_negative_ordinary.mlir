// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-product-tile-with-tail,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s

builtin.module {
  %k0 = "test.arg"() : () -> index
  %k1_nat = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %k1 = "d_tensor.shape.to_index"(%k1_nat) : (!d_tensor.posnat) -> index
  %k = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K:[0-9]+]] = "arith.muli"
// CHECK-NOT: d_tensor.nat.mul
// CHECK: %[[CLAMPED:[0-9]+]] = "arith.minsi"(%{{[0-9]+}}, %[[K]])
// CHECK: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%[[CLAMPED]]) step 1 : i32 iter_args
