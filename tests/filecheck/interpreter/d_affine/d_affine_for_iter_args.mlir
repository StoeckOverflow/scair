// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 5 : index}> : () -> index
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
// IR: %0 = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %1 = "arith.constant"() <{value = 5 : index}> : () -> index
// IR: %2 = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %3 = d_affine.for %4 = #map(%0) to #map(%1) step 1 : i32 iter_args(%5 = %2 : index) {
// IR: %6 = d_affine.apply #map1 (%4)[%5] : (index)[index] -> index
// IR: d_affine.yield %6 : (index)
// IR: }
// IR: func.return %3 : index

// EXEC: Result: 10
