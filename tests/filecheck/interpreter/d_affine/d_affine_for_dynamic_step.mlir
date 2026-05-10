// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %ub_nat = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
    %step_nat = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %lb = "dtensor.shape.to_index"(%lb_nat) : (!dtensor.nat) -> index
    %ub = "dtensor.shape.to_index"(%ub_nat) : (!dtensor.nat) -> index
    %step = "dtensor.shape.to_index"(%step_nat) : (!dtensor.nat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step %step : index iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%acc] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// IR: d_affine.for %{{.*}} = #map(%{{.*}}) to #map(%{{.*}}) step %{{.*}} : index iter_args
// EXEC: Result: 12
