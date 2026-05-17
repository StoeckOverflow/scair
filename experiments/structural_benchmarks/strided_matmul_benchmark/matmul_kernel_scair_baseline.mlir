builtin.module {
  func.func @matmul_strided(
    %n : index,
    %m : index,
    %k : index,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %Aflat : memref<?xf32>,
    %Bflat : memref<?xf32>,
    %Cflat : memref<?xf32>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %A = "memref.reinterpret_cast"(%Aflat, %c0, %n, %k, %a_stride0, %a_stride1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>

    %B = "memref.reinterpret_cast"(%Bflat, %c0, %k, %m, %b_stride0, %b_stride1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>

    %C = "memref.reinterpret_cast"(%Cflat, %c0, %n, %m, %c_stride0, %c_stride1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>

    affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
        %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = "memref.load"(%A, %i, %p) : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
          %b = "memref.load"(%B, %p, %j) : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          affine.yield %next : f32
        }
        "memref.store"(%sum, %C, %i, %j) : (f32, memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
      }
    }
    "func.return"() : () -> ()
  }
}
