// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-dtensor-nat-products | filecheck %s

builtin.module {
  %k0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %k1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %c8 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %c2 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %inner = "dtensor.nat.mul"(%c8, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %outer = "dtensor.nat.mul"(%k0, %inner) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %mixed = "dtensor.nat.mul"(%k1, %c2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  "test.keep"(%outer, %mixed) : (!dtensor.nat, !dtensor.nat) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %[[K1:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %[[C8:[0-9]+]] = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// CHECK: %[[C2:[0-9]+]] = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// CHECK: %[[INNER:[0-9]+]] = "dtensor.nat.mul"(%[[C8]], %[[K1]])
// CHECK: %[[CANON_OUTER_PREFIX:[0-9]+]] = "dtensor.nat.mul"(%[[C8]], %[[K0]])
// CHECK: %[[CANON_OUTER:[0-9]+]] = "dtensor.nat.mul"(%[[CANON_OUTER_PREFIX]], %[[K1]])
// CHECK: %[[CANON_MIXED:[0-9]+]] = "dtensor.nat.mul"(%[[C2]], %[[K1]])
// CHECK: "test.keep"(%[[CANON_OUTER]], %[[CANON_MIXED]])
