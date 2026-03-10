// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-affine-min-simplify | filecheck %s -DFILE=%s --check-prefix=SIMPLIFY
// RUN: ! scair-opt %s --allow-unregistered-dialect -p d-memref-bounds-check 2>&1 | filecheck %s -DFILE=%s --check-prefix=BOUNDS
// Tiled-offset/subview-origin example in the currently supported affine subset.

#tile_i = affine_map<(d0)[s0] -> (d0 + s0)>
#tile_j = affine_map<(d0)[s0] -> (d0 + s0)>

builtin.module {
  %tile = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %c64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
  %c128 = "dtensor.nat.const"() <{value = 128 : i32}> : () -> !dtensor.nat
  %s0_nat = "dtensor.nat.mul"(%tile, %c64) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1_nat = "dtensor.nat.mul"(%tile, %c128) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s0 = "dtensor.shape.to_index"(%s0_nat) : (!dtensor.nat) -> index
  %s1 = "dtensor.shape.to_index"(%s1_nat) : (!dtensor.nat) -> index

  %d0 = "arith.constant"() <{value = 1 : index}> : () -> index
  %d1 = "arith.constant"() <{value = 2 : index}> : () -> index
  %off_i = d_affine.apply #tile_i(%d0)[%s0] : (index)[index] -> index
  %off_j = d_affine.apply #tile_j(%d1)[%s1] : (index)[index] -> index

  %dim0 = "dtensor.nat.const"() <{value = 96 : i32}> : () -> !dtensor.nat
  %dim1 = "dtensor.nat.const"() <{value = 512 : i32}> : () -> !dtensor.nat
  %src = d_memref.alloc : () -> !d_memref.memref<[%dim0, %dim1], i32>
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %sv = d_memref.subview %src[%off_i, %off_j][%s0, %s1][%one, %one] : !d_memref.memref<[%dim0, %dim1], i32> -> !d_memref.memref<[%s0_nat, %s1_nat], i32>
  "test.keep"(%sv) : (!d_memref.memref<[%s0_nat, %s1_nat], i32>) -> ()
}

// SIMPLIFY-LABEL: builtin.module
// SIMPLIFY: "arith.constant"() <{value = 65 : index}> : () -> index
// SIMPLIFY: "arith.constant"() <{value = 130 : index}> : () -> index
// SIMPLIFY-NOT: d_affine.apply
// SIMPLIFY: d_memref.subview

// BOUNDS: d_memref-bounds: `d_memref.subview` axis 0 provably out of bounds (65 + 64 > 96)
