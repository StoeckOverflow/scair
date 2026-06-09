// RUN: ! scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %eight = "arith.constant"() <{value = 8 : index}> : () -> index
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %neg_one = "arith.constant"() <{value = -1 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %src = d_memref.alloc : () -> !d_memref.memref<[%eight], i32>
  %sv = d_memref.subview %src[%neg_one][%zero][%one] : !d_memref.memref<[%eight], i32> -> !d_memref.memref<[%zero], i32>
}

// CHECK: d_memref-bounds: `d_memref.subview` axis 0 provably out of bounds (offset -1 < 0)
