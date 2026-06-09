// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-nat-proofs-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_d_affine_still_present(%k0: index, %out: memref<?xf32>) {
    %k0_nat = "d_tensor.index_to_nat"(%k0) : (index) -> !d_tensor.nat
    %k1_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %k_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %k = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: erase-d-tensor-nat-proofs-to-index cannot run while d_affine.for remains
