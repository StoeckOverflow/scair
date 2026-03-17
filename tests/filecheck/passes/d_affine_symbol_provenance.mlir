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

// CHECK-LABEL: func.func @project_symbol_nat_provenance(%0: !dtensor.nat) -> index {
// CHECK-NEXT:    %1 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %2 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:    %3 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// CHECK-NEXT:    %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    func.return %3 : index
// CHECK-NEXT:  }

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

// CHECK-LABEL: func.func @affine_subset_symbol_const_fold() -> index {
// CHECK-NEXT:    %0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %1 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %2 = "dtensor.nat.const"() <{value = 128 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:    %4 = "dtensor.nat.mul"(%0, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:    %5 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK-NEXT:    %6 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
// CHECK-NEXT:    %7 = "arith.constant"() <{value = 3 : index}> : () -> index
// CHECK-NEXT:    %8 = "arith.constant"() <{value = 5 : index}> : () -> index
// CHECK-NEXT:    %9 = "arith.constant"() <{value = 392 : index}> : () -> index
// CHECK-NEXT:    func.return %9 : index
// CHECK-NEXT:  }
