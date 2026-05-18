builtin.module {
  func.func @nested_commuted_explicit_product_lazy(%out: memref<?xf32>) {
    %k0 = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
    %k1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %k2 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
    %k10 = "dtensor.nat.mul"(%k1, %k0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %k = "dtensor.nat.mul"(%k10, %k2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
