// RUN: scair-run %s | filecheck %s

builtin.module {
  func.func @main() -> index {
    %lb_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %ub_nat = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
    %lb = "dtensor.shape.to_index"(%lb_nat) : (!dtensor.nat) -> index
    %ub = "dtensor.shape.to_index"(%ub_nat) : (!dtensor.nat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%acc] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// CHECK: Result: 10
