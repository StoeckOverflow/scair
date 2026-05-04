builtin.module {
  func.func @broadcast_affine_2d(
    %k0 : index,
    %k1 : index,
    %Xflat : memref<?xi64>,
    %scaleFlat : memref<?xi64>,
    %biasFlat : memref<?xi64>,
    %Yflat : memref<?xi64>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index

    %X = "memref.reinterpret_cast"(%Xflat, %c0, %k0, %k1, %k1, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xi64>, index, index, index, index, index)
        -> memref<?x?xi64, strided<[?, ?], offset: 0>>

    %scale = "memref.reinterpret_cast"(%scaleFlat, %c0, %k1, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 1, 1>}>
      : (memref<?xi64>, index, index, index)
        -> memref<?xi64, strided<[?], offset: 0>>

    %bias = "memref.reinterpret_cast"(%biasFlat, %c0, %k1, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 1, 1>}>
      : (memref<?xi64>, index, index, index)
        -> memref<?xi64, strided<[?], offset: 0>>

    %Y = "memref.reinterpret_cast"(%Yflat, %c0, %k0, %k1, %k1, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xi64>, index, index, index, index, index)
        -> memref<?x?xi64, strided<[?, ?], offset: 0>>

    affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k0) step 1 : index {
      affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k1) step 1 : index {
        %x = "memref.load"(%X, %b, %j) : (memref<?x?xi64, strided<[?, ?], offset: 0>>, index, index) -> i64
        %s = "memref.load"(%scale, %j) : (memref<?xi64, strided<[?], offset: 0>>, index) -> i64
        %bval = "memref.load"(%bias, %j) : (memref<?xi64, strided<[?], offset: 0>>, index) -> i64
        %mul = "arith.muli"(%x, %s) : (i64, i64) -> i64
        %out = "arith.addi"(%mul, %bval) : (i64, i64) -> i64
        "memref.store"(%out, %Y, %b, %j) : (i64, memref<?x?xi64, strided<[?, ?], offset: 0>>, index, index) -> ()
      }
    }
    "func.return"() : () -> ()
  }
}
