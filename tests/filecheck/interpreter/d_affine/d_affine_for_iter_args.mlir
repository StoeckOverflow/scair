// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb_size = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
    %ub_size = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb_size) to affine_map<(d0) -> (d0)>(%ub_size) step 1 : i32 iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%iv, %acc)[] : (index, index)[] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> index {
// IR: %0 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// IR: %1 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// IR: %[[INIT:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %[[SUM:[0-9]+]] = d_affine.for %[[IV:[0-9]+]] = #map(%0) to #map(%1) step 1 : i32 iter_args(%[[ACC:[0-9]+]] = %[[INIT]] : index) {
// IR: %[[NEXT:[0-9]+]] = d_affine.apply #map1 (%[[IV]], %[[ACC]])[] : (index, index)[] -> index
// IR: d_affine.yield %[[NEXT]] : (index)
// IR: }
// IR: func.return %[[SUM]] : index

// EXEC: Result: 10
