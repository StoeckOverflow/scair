// RUN: ! scair-opt %s --allow-unregistered-dialect -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s
// OOB is proven via provenance-recovered symbol constancy in a supported affine expression.

builtin.module {
  %tile = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %c64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
  %sym_nat = "dtensor.nat.mul"(%tile, %c64) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %sym = "dtensor.shape.to_index"(%sym_nat) : (!dtensor.nat) -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %idx = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%one)[%sym] : (index)[index] -> index

  %dim = "dtensor.nat.const"() <{value = 32 : i32}> : () -> !dtensor.nat
  %m = d_memref.alloc : () -> !d_memref.memref<[%dim], i32>
  %r = d_memref.load %m[%idx] : !d_memref.memref<[%dim], i32> -> i32
  "test.keep"(%r) : (i32) -> ()
}

// CHECK: d_memref-bounds: `d_memref.load` index 0 provably out of bounds (65 >= 32)
