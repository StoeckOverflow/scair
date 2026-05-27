// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dtensor-to-dmemref-shape-preserving,dce | filecheck %s -DFILE=%s

// Exact 2D tiling is lowered as a logical view over the original row-major
// storage, not as physical tile packing. For:
//
//   m = mt * tm
//   n = nt * tn
//   [m, n] -> split/split/permute -> [mt, nt, tm, tn]
//
// the index mapping is:
//
//   i = outer_m * tm + inner_m
//   j = outer_n * tn + inner_n
//
// so row-major offset is:
//
//   (outer_m * tm + inner_m) * n + (outer_n * tn + inner_n)
//
// and the logical tiled-view strides are:
//
//   [tm * n, tn, n, 1]

builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.input"() : () -> !dtensor.tensor<[%m, %n], f32>

  %split_m = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %split_n = "dtensor.split_dim"(%split_m) <{dim = 2 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %tile_major = "dtensor.permute_dims"(%split_n)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>)
   -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>

  "test.keep"(%tile_major) : (!dtensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %[[NT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %[[TN:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.mul"(%[[NT]], %[[TN]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %[[SRC:[0-9]+]] = "test.input"() : () -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// CHECK-NEXT:   %[[SRC_MEM:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[SRC]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_memref.memref<[%[[M]], %[[N]]], f32>
// CHECK-NEXT:   %[[TM_IDX:[0-9]+]] = "dtensor.shape.to_index"(%[[TM]]) : (!dtensor.nat) -> index
// CHECK-NEXT:   %[[N_IDX:[0-9]+]] = "dtensor.shape.to_index"(%[[N]]) : (!dtensor.nat) -> index
// CHECK-NEXT:   %[[TN_IDX:[0-9]+]] = "dtensor.shape.to_index"(%[[TN]]) : (!dtensor.nat) -> index
// CHECK-NEXT:   %[[TM_N:[0-9]+]] = "arith.muli"(%[[TM_IDX]], %[[N_IDX]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[SRC_MEM]]
// CHECK-NEXT:   : !d_memref.memref<[%[[M]], %[[N]]], f32> to !d_memref.memref<[%[[MT]], %[[NT]], %[[TM]], %[[TN]]], f32, offset: 0 : index, strides: [%[[TM_N]], %[[TN_IDX]], %[[N_IDX]], 1 : index]>
// CHECK-NEXT:   %[[TENSOR_VIEW:[0-9]+]] = "builtin.unrealized_conversion_cast"(%[[VIEW]]) : (!d_memref.memref<[%[[MT]], %[[NT]], %[[TM]], %[[TN]]], f32, offset: 0 : index, strides: [%[[TM_N]], %[[TN_IDX]], %[[N_IDX]], 1 : index]>) -> !dtensor.tensor<[%[[MT]], %[[NT]], %[[TM]], %[[TN]]], f32>
// CHECK-NEXT:   "test.keep"(%[[TENSOR_VIEW]]) : (!dtensor.tensor<[%[[MT]], %[[NT]], %[[TM]], %[[TN]]], f32>) -> ()
// CHECK-NEXT: }

// -----

// Limitation: a standalone 4D tensor/permute is not enough provenance for this
// lowering. This is intentionally not treated as physical tile packing.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %already_split = "test.already_split"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %tile_major = "dtensor.permute_dims"(%already_split)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>)
   -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>
  "test.keep"(%tile_major) : (!dtensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK-NOT: d_memref.reinterpret_cast
// CHECK: "dtensor.permute_dims"
// CHECK-SAME: permutation = {{\[0 : i32, 2 : i32, 1 : i32, 3 : i32\]}}
// CHECK-NOT: d_memref.reinterpret_cast
// CHECK: }
