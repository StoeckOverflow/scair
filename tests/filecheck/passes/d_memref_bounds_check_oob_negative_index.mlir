// RUN: ! scair-opt %s --allow-unregistered-dialect -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %four = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %neg_one = "arith.constant"() <{value = -1 : index}> : () -> index
  %m = d_memref.alloc : () -> !d_memref.memref<[%four], i32>
  %r = d_memref.load %m[%neg_one] : !d_memref.memref<[%four], i32> -> i32
  "test.keep"(%r) : (i32) -> ()
}

// CHECK: d_memref-bounds: `d_memref.load` index 0 provably out of bounds (-1 < 0)
