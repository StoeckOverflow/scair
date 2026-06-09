// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-nat-proofs-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_d_affine_apply_still_present(%k0: index) -> index {
    %k0_nat = "d_tensor.index_to_nat"(%k0) : (index) -> !d_tensor.nat
    %k1_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %k_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %k = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %shifted = d_affine.apply affine_map<(d0) -> (d0 + 1)>(%k)[] : (index)[] -> index
    "func.return"(%shifted) : (index) -> ()
  }
}

// CHECK: erase-d-tensor-nat-proofs-to-index cannot run while d_affine.apply remains
