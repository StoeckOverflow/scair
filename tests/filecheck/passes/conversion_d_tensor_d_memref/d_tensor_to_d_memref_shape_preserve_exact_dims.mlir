// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving | filecheck %s -DFILE=%s

// Purpose: cover the current shape-preserving subset more completely than the dim-only smoke.

builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
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
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK-NEXT:   %3 = "test.src"() : () -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %4 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %5 = "builtin.unrealized_conversion_cast"(%4) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %6 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:   %7 = "d_tensor.shape.to_index"(%0) : (!d_tensor.nat) -> index
// CHECK-NEXT:   %8 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// CHECK-NEXT:   %9 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   d_affine.for %10 = #map(%6) to #map(%7) step 1 : i32 {
// CHECK-NEXT: d_affine.for %11 = #map(%6) to #map(%8) step 1 : i32 {
// CHECK-NEXT: d_memref.store %2, %9[%10, %11] : i32, !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT: d_affine.yield
// CHECK-NEXT:     }
// CHECK-NEXT: d_affine.yield
// CHECK-NEXT:   }
// CHECK-NEXT:   %12 = "builtin.unrealized_conversion_cast"(%9) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %13 = "builtin.unrealized_conversion_cast"(%3) : (!d_tensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %14 = d_memref.cast %13 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %15 = "builtin.unrealized_conversion_cast"(%14) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %16 = "builtin.unrealized_conversion_cast"(%15) : (!d_tensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %17 = d_memref.dim_exact %16 {axis = 0 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%0>
// CHECK-NEXT:   "test.keep"(%5, %12, %15, %17) : (!d_tensor.tensor<[%0, %1], i32>, !d_tensor.tensor<[%0, %1], i32>, !d_tensor.tensor<[%0, %1], i32>, !value<%0>) -> ()
// CHECK-NEXT: }
