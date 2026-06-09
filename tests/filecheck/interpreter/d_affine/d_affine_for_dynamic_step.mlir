// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> index {
    %lb_size = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
    %ub_size = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
    %step_size = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb_size) to affine_map<(d0) -> (d0)>(%ub_size) step %step_size : index iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%acc] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// IR: d_affine.for %{{.*}} = #map(%{{.*}}) to #map(%{{.*}}) step %{{.*}} : !d_tensor.size iter_args
// EXEC: Result: 12
