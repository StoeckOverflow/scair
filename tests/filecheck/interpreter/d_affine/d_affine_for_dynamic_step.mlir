// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb_nat = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
    %ub_nat = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
    %step_nat = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
    %lb = "d_tensor.shape.to_index"(%lb_nat) : (!d_tensor.nat) -> index
    %ub = "d_tensor.shape.to_index"(%ub_nat) : (!d_tensor.nat) -> index
    %step = "d_tensor.shape.to_index"(%step_nat) : (!d_tensor.nat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step %step : index iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%iv, %acc)[] : (index, index)[] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// IR: d_affine.for %{{.*}} = #map(%{{.*}}) to #map(%{{.*}}) step %{{.*}} : index iter_args
// EXEC: Result: 12
