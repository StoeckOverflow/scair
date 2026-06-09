// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-nat-products | filecheck %s

builtin.module {
  %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %c8 = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %c2 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %inner = "d_tensor.nat.mul"(%c8, %k1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %outer = "d_tensor.nat.mul"(%k0, %inner) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %mixed = "d_tensor.nat.mul"(%k1, %c2) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  "test.keep"(%outer, %mixed) : (!d_tensor.nat, !d_tensor.nat) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK: %[[K1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK: %[[C8:[0-9]+]] = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// CHECK: %[[C2:[0-9]+]] = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// CHECK: %[[INNER:[0-9]+]] = "d_tensor.nat.mul"(%[[C8]], %[[K1]])
// CHECK: %[[CANON_OUTER_PREFIX:[0-9]+]] = "d_tensor.nat.mul"(%[[C8]], %[[K0]])
// CHECK: %[[CANON_OUTER:[0-9]+]] = "d_tensor.nat.mul"(%[[CANON_OUTER_PREFIX]], %[[K1]])
// CHECK: %[[CANON_MIXED:[0-9]+]] = "d_tensor.nat.mul"(%[[C2]], %[[K1]])
// CHECK: "test.keep"(%[[CANON_OUTER]], %[[CANON_MIXED]])
