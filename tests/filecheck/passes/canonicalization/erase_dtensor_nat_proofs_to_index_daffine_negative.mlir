// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-dtensor-nat-proofs-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_daffine_still_present(%k0: index, %out: memref<?xf32>) {
    %k0_nat = "dtensor.index_to_nat"(%k0) : (index) -> !dtensor.nat
    %k1_nat = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: erase-dtensor-nat-proofs-to-index cannot run while d_affine.for remains
