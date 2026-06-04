// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s

// Purpose: cover the current shape-preserving subset more completely than the dim-only smoke.

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %fill = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %src = "test.src"() : () -> !dtensor.tensor<[%m, %n], i32>
  %empty = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], i32>
  %filled = "dtensor.fill"(%fill) : (i32) -> !dtensor.tensor<[%m, %n], i32>
  %cast = "dtensor.cast"(%src) : (!dtensor.tensor<[%m, %n], i32>) -> !dtensor.tensor<[%m, %n], i32>
  %d0 = "dtensor.dim"(%cast) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], i32>) -> !value<%m>
  "test.keep"(%empty, %filled, %cast, %d0) : (!dtensor.tensor<[%m, %n], i32>, !dtensor.tensor<[%m, %n], i32>, !dtensor.tensor<[%m, %n], i32>, !value<%m>) -> ()
}

// CHECK: #map = affine_map<(d0)[] -> (d0)>
// CHECK-NEXT: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK-NEXT:   %3 = "test.src"() : () -> !dtensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %4 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %5 = "builtin.unrealized_conversion_cast"(%4) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %6 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:   %7 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK-NEXT:   %8 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NEXT:   %9 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   d_affine.for %10 = #map(%6) to #map(%7) step 1 : i32 {
// CHECK-NEXT: d_affine.for %11 = #map(%6) to #map(%8) step 1 : i32 {
// CHECK-NEXT: d_memref.store %2, %9[%10, %11] : i32, !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT: d_affine.yield
// CHECK-NEXT:     }
// CHECK-NEXT: d_affine.yield
// CHECK-NEXT:   }
// CHECK-NEXT:   %12 = "builtin.unrealized_conversion_cast"(%9) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %13 = "builtin.unrealized_conversion_cast"(%3) : (!dtensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %14 = d_memref.cast %13 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %15 = "builtin.unrealized_conversion_cast"(%14) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// CHECK-NEXT:   %16 = "builtin.unrealized_conversion_cast"(%15) : (!dtensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK-NEXT:   %17 = d_memref.dim_exact %16 {axis = 0 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%0>
// CHECK-NEXT:   "test.keep"(%5, %12, %15, %17) : (!dtensor.tensor<[%0, %1], i32>, !dtensor.tensor<[%0, %1], i32>, !dtensor.tensor<[%0, %1], i32>, !value<%0>) -> ()
// CHECK-NEXT: }
