// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-nat-proofs-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_d_affine_min_still_present(%k0: index, %k1: index) -> index {
    %k0_nat = "d_tensor.index_to_nat"(%k0) : (index) -> !d_tensor.nat
    %k1_nat = "d_tensor.index_to_nat"(%k1) : (index) -> !d_tensor.nat
    %k_nat = "d_tensor.nat.add"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %k = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %clamped = d_affine.min affine_map<(d0) -> (d0)>(%k)[] : (index)[] -> index
    "func.return"(%clamped) : (index) -> ()
  }
}

// CHECK: erase-d-tensor-nat-proofs-to-index cannot run while d_affine.min remains
