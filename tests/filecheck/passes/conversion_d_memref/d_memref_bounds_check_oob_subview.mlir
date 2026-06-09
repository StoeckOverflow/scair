// RUN: ! scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %eight = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %five = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %four = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %five_idx = "d_tensor.shape.to_index"(%five) : (!d_tensor.nat) -> index
  %four_idx = "d_tensor.shape.to_index"(%four) : (!d_tensor.nat) -> index
  %one_idx = "arith.constant"() <{value = 1 : index}> : () -> index
  %src = d_memref.alloc : () -> !d_memref.memref<[%eight], i32>
  %sv = d_memref.subview %src[%five_idx][%four_idx][%one_idx] : !d_memref.memref<[%eight], i32> -> !d_memref.memref<[%four], i32>
}

// CHECK: d_memref-bounds: `d_memref.subview` axis 0 provably out of bounds (5 + 4 > 8)
