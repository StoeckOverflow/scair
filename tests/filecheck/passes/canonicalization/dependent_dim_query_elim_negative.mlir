// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m], f32>
  %dim = d_memref.dim %buf, %zero : !d_memref.memref<[%m], f32> -> index
  "test.keep"(%dim) : (index) -> ()
}

// CHECK: %[[M:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK: %[[ZERO:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[BUF:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[M]]], f32>
// CHECK: %[[DIM:[0-9]+]] = d_memref.dim %[[BUF]], %[[ZERO]] : !d_memref.memref<[%[[M]]], f32> -> index
// CHECK: "test.keep"(%[[DIM]]) : (index) -> ()
// CHECK-NOT: "d_tensor.dim"
