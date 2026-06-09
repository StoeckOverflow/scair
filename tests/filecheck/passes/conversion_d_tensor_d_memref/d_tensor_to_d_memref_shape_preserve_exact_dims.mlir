// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving | filecheck %s -DFILE=%s

// Purpose: cover the current shape-preserving subset more completely than the dim-only smoke.

builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %fill = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], i32>
  %empty = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, %n], i32>
  %filled = "d_tensor.fill"(%fill) : (i32) -> !d_tensor.tensor<[%m, %n], i32>
  %cast = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%m, %n], i32>) -> !d_tensor.tensor<[%m, %n], i32>
  %d0 = "d_tensor.dim"(%cast) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], i32>) -> !value<%m>
  "test.keep"(%empty, %filled, %cast, %d0) : (!d_tensor.tensor<[%m, %n], i32>, !d_tensor.tensor<[%m, %n], i32>, !d_tensor.tensor<[%m, %n], i32>, !value<%m>) -> ()
}

// CHECK: #map = affine_map<(d0)[] -> (d0)>
// CHECK-NEXT: builtin.module {
// CHECK-NEXT:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK-NEXT:   %1 = "arith.constant"() <{value = 5 : index}> : () -> index
// CHECK-NEXT:   %2 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK-NEXT:   %3 = "test.src"() : () -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %4 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %5 = "builtin.unrealized_conversion_cast"(%4) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %6 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:   %7 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   d_affine.for %8 = #map(%6) to #map(%0) step 1 : i32 {
// CHECK-NEXT: d_affine.for %9 = #map(%6) to #map(%1) step 1 : i32 {
// CHECK-NEXT: d_memref.store %2, %7[%8, %9] : i32, !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT: d_affine.yield
// CHECK-NEXT:     }
// CHECK-NEXT: d_affine.yield
// CHECK-NEXT:   }
// CHECK-NEXT:   %10 = "builtin.unrealized_conversion_cast"(%7) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %11 = "builtin.unrealized_conversion_cast"(%3) : (!d_tensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %12 = d_memref.cast %11 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %13 = "builtin.unrealized_conversion_cast"(%12) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %14 = "builtin.unrealized_conversion_cast"(%13) : (!d_tensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %15 = d_memref.dim_exact %14 {axis = 0 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%0>
// CHECK-NEXT:   "test.keep"(%5, %10, %13, %15) : (!d_tensor.tensor<[%0, %1], i32>, !d_tensor.tensor<[%0, %1], i32>, !d_tensor.tensor<[%0, %1], i32>, !value<%0>) -> ()
// CHECK-NEXT: }
