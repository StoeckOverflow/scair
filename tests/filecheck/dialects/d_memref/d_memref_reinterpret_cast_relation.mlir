// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Purpose: dedicated reinterpret-cast relation coverage for dependent memrefs.

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %n_idx = "d_tensor.shape.to_index"(%n) : (!d_tensor.nat) -> index
  %flat = d_memref.alloc : () -> !d_memref.memref<[%mn], f32>
  %view = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%mn], f32>
      to !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n_idx, %o]>
  %d0 = d_memref.dim_exact %view {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n_idx, %o]> -> !value<%m>
  %d1 = d_memref.dim_exact %view {axis = 1 : i32} : !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n_idx, %o]> -> !value<%n>
  "test.keep"(%view, %d0, %d1)
    : (!d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n_idx, %o]>, !value<%m>, !value<%n>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.mul"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK-NEXT:   %3 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:   %4 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK-NEXT:   %5 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// CHECK-NEXT:   %6 = d_memref.alloc : () -> !d_memref.memref<[%2], f32>
// CHECK-NEXT:   %7 = d_memref.reinterpret_cast %6
// CHECK-NEXT:   : !d_memref.memref<[%2], f32> to !d_memref.memref<[%0, %1], f32, offset: %3, strides: [%5, %4]>
// CHECK-NEXT:   %8 = d_memref.dim_exact %7 {axis = 0 : i32} : !d_memref.memref<[%0, %1], f32, offset: %3, strides: [%5, %4]> -> !value<%0>
// CHECK-NEXT:   %9 = d_memref.dim_exact %7 {axis = 1 : i32} : !d_memref.memref<[%0, %1], f32, offset: %3, strides: [%5, %4]> -> !value<%1>
// CHECK-NEXT:   "test.keep"(%7, %8, %9) : (!d_memref.memref<[%0, %1], f32, offset: %3, strides: [%5, %4]>, !value<%0>, !value<%1>) -> ()
// CHECK-NEXT: }

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %n_idx = "d_tensor.shape.to_index"(%n) : (!d_tensor.nat) -> index
  %flat = d_memref.alloc : () -> !d_memref.memref<[%mn], f32>
  %view = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%mn], f32>
      to !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n_idx, %o]>
  %bad = d_memref.dim_exact %view {axis = 1 : i32} : !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n_idx, %o]> -> !value<%m>
}

// CHECK: d_memref.dim_exact: expected result !value<...> to reference the selected embedded dim
