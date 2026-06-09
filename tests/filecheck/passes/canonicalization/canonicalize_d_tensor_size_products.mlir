// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-size-products | filecheck %s

builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %k1 = "d_tensor.size.param"() : () -> !d_tensor.size
  %c8 = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %c2 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %inner = "d_tensor.size.mul"(%c8, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %outer = "d_tensor.size.mul"(%k0, %inner) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %mixed = "d_tensor.size.mul"(%k1, %c2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  "test.keep"(%outer, %mixed) : (!d_tensor.size, !d_tensor.size) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK: %[[C8:[0-9]+]] = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// CHECK: %[[C2:[0-9]+]] = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// CHECK: %[[INNER:[0-9]+]] = "d_tensor.size.mul"(%[[C8]], %[[K1]])
// CHECK: %[[CANON_OUTER_PREFIX:[0-9]+]] = "d_tensor.size.mul"(%[[C8]], %[[K0]])
// CHECK: %[[CANON_OUTER:[0-9]+]] = "d_tensor.size.mul"(%[[CANON_OUTER_PREFIX]], %[[K1]])
// CHECK: %[[CANON_MIXED:[0-9]+]] = "d_tensor.size.mul"(%[[C2]], %[[K1]])
// CHECK: "test.keep"(%[[CANON_OUTER]], %[[CANON_MIXED]])
