// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-dtensor-nat-proofs-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_daffine_min_still_present(%k0: index, %k1: index) -> index {
    %k0_nat = "dtensor.index_to_nat"(%k0) : (index) -> !dtensor.nat
    %k1_nat = "dtensor.index_to_nat"(%k1) : (index) -> !dtensor.nat
    %k_nat = "dtensor.nat.add"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index
    %clamped = d_affine.min affine_map<(d0) -> (d0)>(%k)[] : (index)[] -> index
    "func.return"(%clamped) : (index) -> ()
  }
}

// CHECK: erase-dtensor-nat-proofs-to-index cannot run while d_affine.min remains
