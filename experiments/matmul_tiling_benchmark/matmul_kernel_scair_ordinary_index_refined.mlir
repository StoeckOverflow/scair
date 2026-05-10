builtin.module {
  func.func @matmul_tiling(
    %m : index,
    %n : index,
    %k0 : index,
    %k1 : index,
    %Aflat : memref<?xf32>,
    %Bflat : memref<?xf32>,
    %Cflat : memref<?xf32>
  ) attributes {llvm.emit_c_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index

    %A = "memref.reinterpret_cast"(%Aflat, %c0, %m, %k, %k, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: ?>>

    %B = "memref.reinterpret_cast"(%Bflat, %c0, %k, %n, %n, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: ?>>

    %C = "memref.reinterpret_cast"(%Cflat, %c0, %m, %n, %n, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: ?>>

    affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
      affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
        %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = "memref.load"(%A, %i, %p) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
          %b = "memref.load"(%B, %p, %j) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          affine.yield %next : f32
        }
        "memref.store"(%sum, %C, %i, %j) : (f32, memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> ()
      }
    }

    "func.return"() : () -> ()
  }
}
