// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products | filecheck %s

builtin.module {
  %k0 = "test.index"() : () -> index
  %k1 = "test.index"() : () -> index
  %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
  %c2 = "arith.constant"() <{value = 2 : index}> : () -> index
  %inner = "arith.muli"(%c8, %k1) : (index, index) -> index
  %outer = "arith.muli"(%k0, %inner) : (index, index) -> index
  %mixed = "arith.muli"(%k1, %c2) : (index, index) -> index
  %t = "test.tensor"() : () -> !d_tensor.tensor<[%outer, %mixed], f32>
  "test.keep"(%t) : (!d_tensor.tensor<[%outer, %mixed], f32>) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "test.index"() : () -> index
// CHECK: %[[K1:[0-9]+]] = "test.index"() : () -> index
// CHECK: %[[C8:[0-9]+]] = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK: %[[C2:[0-9]+]] = "arith.constant"() <{value = 2 : index}> : () -> index
// CHECK: %[[INNER:[0-9]+]] = "arith.muli"(%[[C8]], %[[K1]])
// CHECK: %[[CANON_OUTER_PREFIX:[0-9]+]] = "arith.muli"(%[[C8]], %[[K0]])
// CHECK: %[[CANON_OUTER:[0-9]+]] = "arith.muli"(%[[CANON_OUTER_PREFIX]], %[[K1]])
// CHECK: %[[CANON_MIXED:[0-9]+]] = "arith.muli"(%[[C2]], %[[K1]])
// CHECK: %[[T:[0-9]+]] = "test.tensor"() : () -> !d_tensor.tensor<[%[[CANON_OUTER]], %[[CANON_MIXED]]], f32>
// CHECK: "test.keep"(%[[T]]) : (!d_tensor.tensor<[%[[CANON_OUTER]], %[[CANON_MIXED]]], f32>) -> ()

// -----

// assume_extent can act as a shape root when this pass runs before metadata erasure.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %c4 = "arith.constant"() <{value = 4 : index}> : () -> index
  %inner = "arith.muli"(%c4, %n) : (index, index) -> index
  %prod = "arith.muli"(%m, %inner) : (index, index) -> index
  "d_tensor.assume_extent"(%prod) : (index) -> ()
}

// CHECK: %[[M:[0-9]+]] = "test.index"() : () -> index
// CHECK: %[[N:[0-9]+]] = "test.index"() : () -> index
// CHECK: %[[C4:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[INNER2:[0-9]+]] = "arith.muli"(%[[C4]], %[[N]])
// CHECK: %[[CANON_ASSUME_PREFIX:[0-9]+]] = "arith.muli"(%[[C4]], %[[M]])
// CHECK: %[[CANON_ASSUME:[0-9]+]] = "arith.muli"(%[[CANON_ASSUME_PREFIX]], %[[N]])
// CHECK: "d_tensor.assume_extent"(%[[CANON_ASSUME]]) : (index) -> ()
