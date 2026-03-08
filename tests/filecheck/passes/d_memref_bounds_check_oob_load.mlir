// RUN: ! scair-opt %s --allow-unregistered-dialect -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %four = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %five = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %five_idx = "dtensor.shape.to_index"(%five) : (!dtensor.nat) -> index
  %m = d_memref.alloc : () -> !d_memref.memref<[%four], i32>
  %r = d_memref.load %m[%five_idx] : !d_memref.memref<[%four], i32> -> i32
  "test.keep"(%r) : (i32) -> ()
}

// CHECK: d_memref-bounds: `d_memref.load` index 0 provably out of bounds (5 >= 4)
