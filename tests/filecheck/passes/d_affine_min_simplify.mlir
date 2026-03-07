// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p d-affine-min-simplify | filecheck %s -DFILE=%s

builtin.module {
  func.func @min_id(%x: !dtensor.nat) -> !dtensor.nat {
    %m = d_affine.min %x, %x : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    func.return %m : !dtensor.nat
  }
}
// CHECK: func.func @min_id
// CHECK-NOT: d_affine.min
// CHECK: func.return %0 : !dtensor.nat

// -----

builtin.module {
  func.func @min_fold_const() -> !dtensor.nat {
    %a = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %b = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %m = d_affine.min %a, %b : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    func.return %m : !dtensor.nat
  }
}
// CHECK: %2 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// CHECK-NOT: d_affine.min

// -----

builtin.module {
  func.func @min_zero(%x: !dtensor.nat) -> !dtensor.nat {
    %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %m = d_affine.min %x, %z : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    func.return %m : !dtensor.nat
  }
}
// CHECK: "dtensor.nat.const"() <{value = 0 : i32}>
// CHECK-NOT: d_affine.min
