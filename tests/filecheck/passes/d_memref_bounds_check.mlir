// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check | filecheck %s -DFILE=%s

builtin.module {
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %i = "dtensor.nat.param"() : () -> !dtensor.nat
  %i_idx = "dtensor.shape.to_index"(%i) : (!dtensor.nat) -> index
  %m = d_memref.alloc : () -> !d_memref.memref<[%n], i32>
  %z = "arith.constant"() <{value = 0 : i32}> : () -> i32
  d_memref.store %z, %m[%i_idx] : i32, !d_memref.memref<[%n], i32>
  d_memref.dealloc %m : !d_memref.memref<[%n], i32>
}
// CHECK: d_memref.store
