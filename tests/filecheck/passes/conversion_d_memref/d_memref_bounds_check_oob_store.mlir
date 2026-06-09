// RUN: ! scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %five = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %src = d_memref.alloc : () -> !d_memref.memref<[%five], i32>
  %v = "arith.constant"() <{value = 0 : i32}> : () -> i32
  d_memref.store %v, %src[%five] : i32, !d_memref.memref<[%five], i32>
}

// CHECK: d_memref-bounds: `d_memref.store` index 0 provably out of bounds (5 >= 5)

