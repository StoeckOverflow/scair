// RUN: ! scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %eight = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %zero_size = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %neg_one = "arith.constant"() <{value = -1 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %src = d_memref.alloc : () -> !d_memref.memref<[%eight], i32>
  %sv = d_memref.subview %src[%neg_one][%zero_size][%one] : !d_memref.memref<[%eight], i32> -> !d_memref.memref<[%zero_size], i32>
}

// CHECK: d_memref-bounds: `d_memref.subview` axis 0 provably out of bounds (offset -1 < 0)
