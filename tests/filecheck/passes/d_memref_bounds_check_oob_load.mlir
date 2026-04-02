// RUN: ! scair-opt %s --allow-unregistered-dialect -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %eight = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %five = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %five_idx = "dtensor.shape.to_index"(%five) : (!dtensor.nat) -> index
  %src = d_memref.alloc : () -> !d_memref.memref<[%eight], i32>
  %r = d_memref.load %src[%five_idx] : !d_memref.memref<[%five], i32> -> i32
}

// CHECK: d_memref-bounds: `d_memref.load` index 0 provably out of bounds (5 >= 5)
