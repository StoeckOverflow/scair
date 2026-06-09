// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,d-tensor-to-d-memref-shape-preserving | filecheck %s -DFILE=%s --check-prefix=LOWER

// Canonicalized collapse_shape lowers to a reinterpret_cast with product-shaped result dims.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %q = "arith.constant"() <{value = 20 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %flat = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%q], f32>
  "test.keep"(%flat) : (!d_tensor.tensor<[%q], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[M:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// LOWER-NEXT:   %[[N:[0-9]+]] = "arith.constant"() <{value = 5 : index}> : () -> index
// LOWER-NEXT:   %[[Q:[0-9]+]] = "arith.constant"() <{value = 20 : index}> : () -> index
// LOWER-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   %[[MN:[0-9]+]] = "arith.muli"(%[[M]], %[[N]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[A]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_memref.memref<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[M]], %[[N]]], f32> to !d_memref.memref<[%[[MN]]], f32, offset: 0 : index, strides: [1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[MN]]], f32, offset: 0 : index, strides: [1 : index]>) -> !d_tensor.tensor<[%[[MN]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!d_tensor.tensor<[%[[MN]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Canonicalized join_dim lowers similarly.
builtin.module {
  %mt = "arith.constant"() <{value = 4 : index}> : () -> index
  %tm = "arith.constant"() <{value = 5 : index}> : () -> index
  %n = "arith.constant"() <{value = 6 : index}> : () -> index
  %q = "arith.constant"() <{value = 20 : index}> : () -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%q, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%q, %n], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[MT:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// LOWER-NEXT:   %[[TM:[0-9]+]] = "arith.constant"() <{value = 5 : index}> : () -> index
// LOWER-NEXT:   %[[N:[0-9]+]] = "arith.constant"() <{value = 6 : index}> : () -> index
// LOWER-NEXT:   %[[Q:[0-9]+]] = "arith.constant"() <{value = 20 : index}> : () -> index
// LOWER-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// LOWER-NEXT:   %[[M:[0-9]+]] = "arith.muli"(%[[MT]], %[[TM]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[B]]) : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !d_memref.memref<[%[[MT]], %[[TM]], %[[N]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[MT]], %[[TM]], %[[N]]], f32> to !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>) -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Product-backed expand_shape lowers; the product fact belongs to the source collapsed dim.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %mn = "arith.muli"(%m, %n) : (index, index) -> index
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %a = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%a) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[M:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// LOWER-NEXT:   %[[N:[0-9]+]] = "arith.constant"() <{value = 5 : index}> : () -> index
// LOWER-NEXT:   %[[MN:[0-9]+]] = "arith.muli"(%[[M]], %[[N]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// LOWER-NEXT:   %[[FLAT:[0-9]+]] = "test.flat"() : () -> !d_tensor.tensor<[%[[MN]]], f32>
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[FLAT]]) : (!d_tensor.tensor<[%[[MN]]], f32>) -> !d_memref.memref<[%[[MN]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[MN]]], f32> to !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0 : index, strides: [%[[N]], 1 : index]>) -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Product-backed split_dim lowers; missing product facts would leave the op unlowered.
builtin.module {
  %mt = "arith.constant"() <{value = 4 : index}> : () -> index
  %tm = "arith.constant"() <{value = 5 : index}> : () -> index
  %m = "arith.muli"(%mt, %tm) : (index, index) -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%mt, %tm], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[MT:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// LOWER-NEXT:   %[[TM:[0-9]+]] = "arith.constant"() <{value = 5 : index}> : () -> index
// LOWER-NEXT:   %[[M:[0-9]+]] = "arith.muli"(%[[MT]], %[[TM]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// LOWER-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]]], f32>
// LOWER-NEXT:   %[[MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[A]]) : (!d_tensor.tensor<[%[[M]]], f32>) -> !d_memref.memref<[%[[M]]], f32>
// LOWER-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[MEM]]
// LOWER-NEXT:   : !d_memref.memref<[%[[M]]], f32> to !d_memref.memref<[%[[MT]], %[[TM]]], f32, offset: 0 : index, strides: [%[[TM]], 1 : index]>
// LOWER-NEXT:   %[[BACK:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[MT]], %[[TM]]], f32, offset: 0 : index, strides: [%[[TM]], 1 : index]>) -> !d_tensor.tensor<[%[[MT]], %[[TM]]], f32>
// LOWER-NEXT:   "test.keep"(%[[BACK]]) : (!d_tensor.tensor<[%[[MT]], %[[TM]]], f32>) -> ()
// LOWER-NEXT: }

// -----

// Missing product facts for split_dim are intentionally not invented by lowering.
builtin.module {
  %m = "arith.constant"() <{value = 20 : index}> : () -> index
  %mt = "arith.constant"() <{value = 4 : index}> : () -> index
  %tm = "arith.constant"() <{value = 5 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%mt, %tm], f32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %[[M:[0-9]+]] = "arith.constant"() <{value = 20 : index}> : () -> index
// LOWER-NEXT:   %[[MT:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// LOWER-NEXT:   %[[TM:[0-9]+]] = "arith.constant"() <{value = 5 : index}> : () -> index
// LOWER-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]]], f32>
// LOWER-NEXT:   %[[SPLIT:[0-9]+]] = "d_tensor.split_dim"(%[[A]], %[[MT]], %[[TM]]) <{dim = 0 : i32}> : (!d_tensor.tensor<[%[[M]]], f32>, index, index) -> !d_tensor.tensor<[%[[MT]], %[[TM]]], f32>
// LOWER-NEXT:   "test.keep"(%[[SPLIT]]) : (!d_tensor.tensor<[%[[MT]], %[[TM]]], f32>) -> ()
// LOWER-NEXT: }
