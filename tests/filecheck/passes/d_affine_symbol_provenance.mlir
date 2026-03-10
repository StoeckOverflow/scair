// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p d-affine-min-simplify | filecheck %s -DFILE=%s
// This test demonstrates provenance-aware symbol recovery and constant folding in the
// currently supported affine subset.
// Note: general semi-affine products like (d0 * s0 + d1 * s1) are not parsed yet;
// multiplication is supported only with a constant operand.

builtin.module {
  func.func @project_symbol_nat_provenance(%tile: !dtensor.nat) -> index {
    %c64_nat = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
    %s0_nat = "dtensor.nat.mul"(%tile, %c64_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %s0 = "dtensor.shape.to_index"(%s0_nat) : (!dtensor.nat) -> index
    %d0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %r = d_affine.apply affine_map<(d0)[s0] -> (s0)>(%d0)[%s0] : (index)[index] -> index
    func.return %r : index
  }
}

// CHECK-LABEL: func.func @project_symbol_nat_provenance
// CHECK: "dtensor.shape.to_index"
// CHECK-NOT: d_affine.apply
// CHECK: func.return %{{.*}} : index

// -----

builtin.module {
  func.func @affine_subset_symbol_const_fold() -> index {
    %tile = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %c64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
    %c128 = "dtensor.nat.const"() <{value = 128 : i32}> : () -> !dtensor.nat
    %s0_nat = "dtensor.nat.mul"(%tile, %c64) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %s1_nat = "dtensor.nat.mul"(%tile, %c128) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %s0 = "dtensor.shape.to_index"(%s0_nat) : (!dtensor.nat) -> index
    %s1 = "dtensor.shape.to_index"(%s1_nat) : (!dtensor.nat) -> index
    %d0 = "arith.constant"() <{value = 3 : index}> : () -> index
    %d1 = "arith.constant"() <{value = 5 : index}> : () -> index
    %idx = d_affine.apply affine_map<(d0, d1)[s0, s1] -> (d0 + d1 + s0 + s1)>(%d0, %d1)[%s0, %s1] : (index, index)[index, index] -> index
    func.return %idx : index
  }
}

// CHECK-LABEL: func.func @affine_subset_symbol_const_fold
// CHECK: "arith.constant"() <{value = 392 : index}> : () -> index
// CHECK-NOT: d_affine.apply
