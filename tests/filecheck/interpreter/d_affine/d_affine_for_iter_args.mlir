// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb_nat = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
    %ub_nat = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
    %lb = "d_tensor.shape.to_index"(%lb_nat) : (!d_tensor.nat) -> index
    %ub = "d_tensor.shape.to_index"(%ub_nat) : (!d_tensor.nat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%acc] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> index {
// IR: %0 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// IR: %1 = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
// IR: %2 = "d_tensor.shape.to_index"(%0) : (!d_tensor.nat) -> index
// IR: %3 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// IR: %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %5 = d_affine.for %6 = #map(%2) to #map(%3) step 1 : i32 iter_args(%7 = %4 : index) {
// IR: %8 = d_affine.apply #map1 (%6)[%7] : (index)[index] -> index
// IR: d_affine.yield %8 : (index)
// IR: }
// IR: func.return %5 : index

// EXEC: Result: 10
