// RUN: ! scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %eight = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %neg_one = "arith.constant"() <{value = -1 : index}> : () -> index
  %src = d_memref.alloc : () -> !d_memref.memref<[%eight], i32>
  %r = d_memref.load %src[%neg_one] : !d_memref.memref<[%eight], i32> -> i32
}

// CHECK: d_memref-bounds: `d_memref.load` index 0 provably out of bounds (-1 < 0)
