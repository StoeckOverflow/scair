// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Purpose: dedicated reinterpret-cast relation coverage for dependent memrefs.

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %flat = d_memref.alloc : () -> !d_memref.memref<[%mn], f32>
  %view = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%mn], f32>
      to !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %o]>
  %d0 = d_memref.dim_exact %view {axis = 0 : i32} : !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %o]> -> !value<%m>
  %d1 = d_memref.dim_exact %view {axis = 1 : i32} : !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %o]> -> !value<%n>
  "test.keep"(%view, %d0, %d1)
    : (!d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %o]>, !value<%m>, !value<%n>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.mul"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %3 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:   %4 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK-NEXT:   %5 = d_memref.alloc : () -> !d_memref.memref<[%2], f32>
// CHECK-NEXT:   %6 = d_memref.reinterpret_cast %5
// CHECK-NEXT:   : !d_memref.memref<[%2], f32> to !d_memref.memref<[%0, %1], f32, offset: %3, strides: [%1, %4]>
// CHECK-NEXT:   %7 = d_memref.dim_exact %6 {axis = 0 : i32} : !d_memref.memref<[%0, %1], f32, offset: %3, strides: [%1, %4]> -> !value<%0>
// CHECK-NEXT:   %8 = d_memref.dim_exact %6 {axis = 1 : i32} : !d_memref.memref<[%0, %1], f32, offset: %3, strides: [%1, %4]> -> !value<%1>
// CHECK-NEXT:   "test.keep"(%6, %7, %8) : (!d_memref.memref<[%0, %1], f32, offset: %3, strides: [%1, %4]>, !value<%0>, !value<%1>) -> ()
// CHECK-NEXT: }

// -----

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %flat = d_memref.alloc : () -> !d_memref.memref<[%mn], f32>
  %view = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%mn], f32>
      to !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %o]>
  %bad = d_memref.dim_exact %view {axis = 1 : i32} : !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %o]> -> !value<%m>
}

// CHECK: d_memref.dim_exact: expected result !value<...> to reference the selected embedded dim
