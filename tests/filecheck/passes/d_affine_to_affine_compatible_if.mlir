// RUN: scair-opt %s --allow-unregistered-dialect -p d-affine-to-affine-compatible | filecheck %s

builtin.module {
  func.func @if_no_results(%i: index) {
    "d_affine.if"(%i) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
      d_affine.yield
    }, {
      d_affine.yield
    }) : (index) -> ()
    func.return
  }

  func.func @if_results(%i: index, %init: index) -> index {
    %r = "d_affine.if"(%i) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
      d_affine.yield %i : (index)
    }, {
      d_affine.yield %init : (index)
    }) : (index) -> index
    func.return %r : index
  }

  func.func @nested_loop_in_if(%i: index, %lb: index, %ub: index) {
    "d_affine.if"(%i) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
      d_affine.for %j = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
        "test.touch"(%j) : (index) -> ()
        d_affine.yield
      }
      d_affine.yield
    }, {
      d_affine.yield
    }) : (index) -> ()
    func.return
  }
}

// CHECK-LABEL: func.func @if_no_results
// CHECK: "affine.if"(%{{[0-9]+}}) <{condition = #set}> ({
// CHECK: affine.yield
// CHECK: }, {
// CHECK: affine.yield
// CHECK: }) : (index) -> ()

// CHECK-LABEL: func.func @if_results
// CHECK: %[[R:[0-9]+]] = "affine.if"(%{{[0-9]+}}) <{condition = #set}> ({
// CHECK: affine.yield %{{[0-9]+}} : index
// CHECK: }, {
// CHECK: affine.yield %{{[0-9]+}} : index
// CHECK: }) : (index) -> index
// CHECK: func.return %[[R]] : index

// CHECK-LABEL: func.func @nested_loop_in_if
// CHECK: "affine.if"(%{{[0-9]+}}) <{condition = #set}> ({
// CHECK: affine.for %[[J:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1
// CHECK: "test.touch"(%[[J]])
// CHECK: affine.yield
// CHECK: }, {
// CHECK: affine.yield
// CHECK: }) : (index) -> ()
// CHECK-NOT: d_affine.if
