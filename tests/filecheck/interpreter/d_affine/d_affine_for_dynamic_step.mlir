// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 7 : index}> : () -> index
    %step = "arith.constant"() <{value = 2 : index}> : () -> index
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
