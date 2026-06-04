// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s --check-prefix=LOWER

// Canonicalized collapse_shape lowers to a reinterpret_cast with product-shaped result dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %flat = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%q], f32>
  "test.keep"(%flat) : (!dtensor.tensor<[%q], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[Q:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   %[[MN:[0-9]+]] = "dtensor.nat.mul"(%[[M]], %[[N]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[A]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_memref.memref<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[M]], %[[N]]], f32> to !d_memref.memref<[%[[MN]]], f32, offset: 0 : index, strides: [1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[MN]]], f32, offset: 0 : index, strides: [1 : index]>) -> !dtensor.tensor<[%[[MN]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!dtensor.tensor<[%[[MN]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Canonicalized join_dim lowers similarly.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %c = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%q, %n], f32>
  "test.keep"(%c) : (!dtensor.tensor<[%q, %n], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[Q:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !dtensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// LOWER-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[B]]) : (!dtensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !d_memref.memref<[%[[MT]], %[[TM]], %[[N]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[MT]], %[[TM]], %[[N]]], f32> to !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>) -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Product-backed expand_shape lowers; the product fact belongs to the source collapsed dim.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %flat = "test.flat"() : () -> !dtensor.tensor<[%mn], f32>
  %a = "dtensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%a) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[MN:[0-9]+]] = "dtensor.nat.mul"(%[[M]], %[[N]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// LOWER-NEXT:   %[[FLAT:[0-9]+]] = "test.flat"() : () -> !dtensor.tensor<[%[[MN]]], f32>
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[FLAT]]) : (!dtensor.tensor<[%[[MN]]], f32>) -> !d_memref.memref<[%[[MN]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[MN]]], f32> to !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>) -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Product-backed split_dim lowers; missing product facts would leave the op unlowered.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "dtensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!dtensor.tensor<[%mt, %tm], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// LOWER-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !dtensor.tensor<[%[[M]]], f32>
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[A]]) : (!dtensor.tensor<[%[[M]]], f32>) -> !d_memref.memref<[%[[M]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[M]]], f32> to !d_memref.memref<[%[[MT]], %[[TM]]], f32, offset: 0 : index, strides: [%[[TM]], 1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[MT]], %[[TM]]], f32, offset: 0 : index, strides: [%[[TM]], 1 : index]>) -> !dtensor.tensor<[%[[MT]], %[[TM]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!dtensor.tensor<[%[[MT]], %[[TM]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Missing product facts for split_dim are intentionally not invented by lowering.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "dtensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!dtensor.tensor<[%mt, %tm], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !dtensor.tensor<[%[[M]]], f32>
// LOWER-NEXT:   %[[SPLIT:[0-9]+]] = "dtensor.split_dim"(%[[A]], %[[MT]], %[[TM]]) <{dim = 0 : i32}> : (!dtensor.tensor<[%[[M]]], f32>, !dtensor.nat, !dtensor.nat) -> !dtensor.tensor<[%[[MT]], %[[TM]]], f32>
// LOWER-NEXT:   "test.keep"(%[[SPLIT]]) : (!dtensor.tensor<[%[[MT]], %[[TM]]], f32>) -> ()
// LOWER-NEXT: }
