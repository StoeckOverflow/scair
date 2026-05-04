builtin.module {
  func.func @broadcast_affine_2d(
    %k0_nat : !dtensor.nat,
    %k1_nat : !dtensor.nat,
    %Xflat : !d_memref.memref<[], i64>,
    %scaleFlat : !d_memref.memref<[], i64>,
    %biasFlat : !d_memref.memref<[], i64>,
    %Yflat : !d_memref.memref<[], i64>
  ) attributes {scair.emit_bare_interface = true} {
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %k0 = "dtensor.shape.to_index"(%k0_nat) : (!dtensor.nat) -> index
    %k1 = "dtensor.shape.to_index"(%k1_nat) : (!dtensor.nat) -> index

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[], i64>
        to !d_memref.memref<[%k_nat], i64, offset: 0, strides: [%c1]>

    %scale = d_memref.reinterpret_cast %scaleFlat
      : !d_memref.memref<[], i64>
        to !d_memref.memref<[%k1_nat], i64, offset: 0, strides: [%c1]>

    %bias = d_memref.reinterpret_cast %biasFlat
      : !d_memref.memref<[], i64>
        to !d_memref.memref<[%k1_nat], i64, offset: 0, strides: [%c1]>

    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], i64>
        to !d_memref.memref<[%k_nat], i64, offset: 0, strides: [%c1]>

    d_affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k0) step 1 : index {
      %base = "arith.muli"(%b, %k1) : (index, index) -> index
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k1) step 1 : index {
        %i = "arith.addi"(%base, %j) : (index, index) -> index
        %x = d_memref.load %X[%i] : !d_memref.memref<[%k_nat], i64, offset: 0, strides: [%c1]> -> i64
        %s = d_memref.load %scale[%j] : !d_memref.memref<[%k1_nat], i64, offset: 0, strides: [%c1]> -> i64
        %bval = d_memref.load %bias[%j] : !d_memref.memref<[%k1_nat], i64, offset: 0, strides: [%c1]> -> i64
        %mul = "arith.muli"(%x, %s) : (i64, i64) -> i64
        %out = "arith.addi"(%mul, %bval) : (i64, i64) -> i64
        d_memref.store %out, %Y[%i] : i64, !d_memref.memref<[%k_nat], i64, offset: 0, strides: [%c1]>
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
