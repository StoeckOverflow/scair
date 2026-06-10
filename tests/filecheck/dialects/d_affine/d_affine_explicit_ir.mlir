// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | filecheck %s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 8 : index}> : () -> index
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  %sym = "arith.constant"() <{value = 8 : index}> : () -> index
  %step = "arith.constant"() <{value = 2 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index
  %value = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  %applied = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%lb, %sym)[] : (index, index)[] -> index
  %minimum = d_affine.min affine_map<(d0)[s0] -> (d0, s0, d0 + s0)>(%applied)[%sym] : (index)[index] -> index
  %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
    %inner = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%iv, %acc)[] : (index, index)[] -> index
    d_affine.yield %inner : (index)
  }
  d_affine.for %dyn = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
    d_affine.yield
  }
  "d_affine.store"(%value, %buf, %lb, %applied) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (i32, !d_memref.memref<[%m, %n], i32>, index, index) -> ()
  %loaded = "d_affine.load"(%buf, %lb, %applied) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (!d_memref.memref<[%m, %n], i32>, index, index) -> i32
  "d_affine.if"(%lb) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
  ^then:
    d_affine.yield
  }, {
  ^else:
    d_affine.yield
  }) : (index) -> ()
  %if_result = "d_affine.if"(%lb) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
  ^then_res:
    d_affine.yield %sum : (index)
  }, {
  ^else_res:
    d_affine.yield %minimum : (index)
  }) : (index) -> index
  // d_affine.parallel is verified as a small no-reduction subset.
  "d_affine.parallel"(%ub) <{
    lowerBoundsMap = affine_map<()[s0] -> (0)>,
    lowerBoundsGroups = dense<1> : vector<1xi32>,
    upperBoundsMap = affine_map<()[s0] -> (s0)>,
    upperBoundsGroups = dense<1> : vector<1xi32>,
    steps = [1 : i64],
    reductions = []
  }> ({
  ^par(%p: index):
  }) : (index) -> ()
  "test.keep"(%applied, %minimum, %sum, %loaded, %if_result) : (index, index, index, i32, index) -> ()
}

// CHECK: builtin.module {
// CHECK:        %[[BUF:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%{{[0-9]+}}, %{{[0-9]+}}], i32>
// CHECK:        %[[APPLIED:[0-9]+]] = d_affine.apply #[[MAP0:map[0-9]*]] (%{{[0-9]+}}, %{{[0-9]+}})[] : (index, index)[] -> index
// CHECK:        %[[MIN:[0-9]+]] = d_affine.min #[[MAP1:map[0-9]*]] (%[[APPLIED]])[%{{[0-9]+}}] : (index)[index] -> index
// CHECK:        %[[SUM:[0-9]+]] = d_affine.for %{{[0-9]+}} = #[[MAP2:map[0-9]*]](%{{[0-9]+}}) to #[[MAP2]](%{{[0-9]+}}) step 1 : i32 iter_args(%{{[0-9]+}} = %{{[0-9]+}} : index) {
// CHECK:          %{{[0-9]+}} = d_affine.apply #[[MAP0]] (%{{[0-9]+}}, %{{[0-9]+}})[] : (index, index)[] -> index
// CHECK:          d_affine.yield %{{[0-9]+}} : (index)
// CHECK:        d_affine.for %{{[0-9]+}} = #[[MAP2]](%{{[0-9]+}}) to #[[MAP2]](%{{[0-9]+}}) step %{{[0-9]+}} : index {
// CHECK:          d_affine.yield
// CHECK:        "d_affine.store"(%{{[0-9]+}}, %[[BUF]], %{{[0-9]+}}, %[[APPLIED]]) <{map = #[[MAP3:map[0-9]*]]}> : (i32, !d_memref.memref<[%{{[0-9]+}}, %{{[0-9]+}}], i32>, index, index) -> ()
// CHECK:        %[[LOADED:[0-9]+]] = "d_affine.load"(%[[BUF]], %{{[0-9]+}}, %[[APPLIED]]) <{map = #[[MAP3]]}> : (!d_memref.memref<[%{{[0-9]+}}, %{{[0-9]+}}], i32>, index, index) -> i32
// CHECK:        "d_affine.if"(%{{[0-9]+}}) <{condition = #set}> ({
// CHECK:          d_affine.yield
// CHECK:        %[[IF_RESULT:[0-9]+]] = "d_affine.if"(%{{[0-9]+}}) <{condition = #set}> ({
// CHECK:          d_affine.yield %[[SUM]] : (index)
// CHECK:          d_affine.yield %[[MIN]] : (index)
// CHECK:        "d_affine.parallel"(%{{[0-9]+}}) <{
// CHECK:        "test.keep"(%[[APPLIED]], %[[MIN]], %[[SUM]], %[[LOADED]], %[[IF_RESULT]]) : (index, index, index, i32, index) -> ()
// CHECK:      }
