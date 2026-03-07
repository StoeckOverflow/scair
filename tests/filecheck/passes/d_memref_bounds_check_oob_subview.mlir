// RUN: ! scair-opt %s --allow-unregistered-dialect -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s

builtin.module {
  %eight = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %five = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %four = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %src = d_memref.alloc : () -> !d_memref.memref<[%eight], i32>
  %sv = d_memref.subview %src[%five][%four] : !d_memref.memref<[%eight], i32> -> !d_memref.memref<[%four], i32>
  "test.keep"(%sv) : (!d_memref.memref<[%four], i32>) -> ()
}

// CHECK: d_memref-bounds: `d_memref.subview` axis 0 provably out of bounds (5 + 4 > 8)
