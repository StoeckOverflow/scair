// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-dtensor-nat-proofs-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_daffine_apply_still_present(%k0: index) -> index {
    %k0_nat = "dtensor.index_to_nat"(%k0) : (index) -> !dtensor.nat
    %k1_nat = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index
    %shifted = d_affine.apply affine_map<(d0) -> (d0 + 1)>(%k)[] : (index)[] -> index
    "func.return"(%shifted) : (index) -> ()
  }
}

// CHECK: erase-dtensor-nat-proofs-to-index cannot run while d_affine.apply remains
