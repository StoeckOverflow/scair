// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s

builtin.module {
  %k0 = "test.index"() : () -> index
  %k1 = "arith.constant"() <{value = 4 : index}> : () -> index
  %other = "test.index"() : () -> index
  %k = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step %k1 : index iter_args(%acc0 = %init : index) {
    %clamped = d_affine.min affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>(%tile)[%k1, %other] : (index)[index, index] -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc1)[] : (index, index)[] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: #[[ID:.*]] = affine_map<(d0)[] -> (d0)>
// CHECK: #[[BAD_MIN:.*]] = affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>
// CHECK: #[[ADD_ACC:.*]] = affine_map<(d0, d1)[] -> (d0 + d1)>
// CHECK: %[[K0:[0-9]+]] = "test.index"() : () -> index
// CHECK: %[[K1:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[OTHER:[0-9]+]] = "test.index"() : () -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%[[K0]], %[[K1]]) {{.*}} : (index, index) -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #[[ID]](%{{[0-9]+}}) to #[[ID]](%[[K]]) step %[[K1]] : index iter_args(%[[ACC0:[0-9]+]] = %{{[0-9]+}} : index) {
// CHECK: %[[CLAMPED:[0-9]+]] = d_affine.min #[[BAD_MIN]] (%[[TILE]])[%[[K1]], %[[OTHER]]] : (index)[index, index] -> index
// CHECK: %[[INNER:[0-9]+]] = d_affine.for %[[P:[0-9]+]] = #[[ID]](%[[TILE]]) to #[[ID]](%[[CLAMPED]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: %[[NEXT:[0-9]+]] = d_affine.apply #[[ADD_ACC]] (%[[P]], %[[ACC1]])[] : (index, index)[] -> index
// CHECK: d_affine.yield %[[NEXT]] : (index)
// CHECK: d_affine.yield %[[INNER]] : (index)
// CHECK: "test.keep"(%[[SUM]]) : (index) -> ()
