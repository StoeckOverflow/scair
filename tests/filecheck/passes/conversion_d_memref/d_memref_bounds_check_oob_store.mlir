// RUN: ! scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %five = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %five_idx = "d_tensor.shape.to_index"(%five) : (!d_tensor.nat) -> index
  %src = d_memref.alloc : () -> !d_memref.memref<[%five], i32>
  %v = "arith.constant"() <{value = 0 : i32}> : () -> i32
  d_memref.store %v, %src[%five_idx] : i32, !d_memref.memref<[%five], i32>
}

// CHECK: d_memref-bounds: `d_memref.store` index 0 provably out of bounds (5 >= 5)

