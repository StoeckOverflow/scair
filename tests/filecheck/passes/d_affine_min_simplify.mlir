// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p d-affine-min-simplify | filecheck %s -DFILE=%s

builtin.module {
  func.func @min_id(%x: index) -> index {
    %m = d_affine.min %x, %x : (index, index) -> index
    func.return %m : index
  }
}
// CHECK: func.func @min_id
// CHECK-NOT: d_affine.min
// CHECK: func.return %0 : index

// -----

builtin.module {
  func.func @min_fold_const() -> index {
    %a = "arith.constant"() <{value = 8 : index}> : () -> index
    %b = "arith.constant"() <{value = 4 : index}> : () -> index
    %m = d_affine.min %a, %b : (index, index) -> index
    func.return %m : index
  }
}
// CHECK: "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK-NOT: d_affine.min

// -----

builtin.module {
  func.func @min_zero(%x: index) -> index {
    %z = "arith.constant"() <{value = 0 : index}> : () -> index
    %m = d_affine.min %x, %z : (index, index) -> index
    func.return %m : index
  }
}
// CHECK: "arith.constant"() <{value = 0 : index}>
// CHECK-NOT: d_affine.min

// -----

builtin.module {
  func.func @min_same_nat_provenance() -> index {
    %n = "dtensor.nat.const"() <{value = 42 : i32}> : () -> !dtensor.nat
    %a = "dtensor.shape.to_index"(%n) : (!dtensor.nat) -> index
    %b = "dtensor.shape.to_index"(%n) : (!dtensor.nat) -> index
    %m = d_affine.min %a, %b : (index, index) -> index
    func.return %m : index
  }
}
// CHECK: func.func @min_same_nat_provenance
// CHECK-NOT: d_affine.min
