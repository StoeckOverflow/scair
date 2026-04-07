builtin.module {
  func.func @semi_affine_fill_and_sum(
    %stride0 : index,
    %stride1 : index,
    %flat : memref<?xf32>,
    %out : memref<1xf32>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %buf = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{
      operandSegmentSizes = array<i32: 1, 1, 2, 2>
    }> : (memref<?xf32>, index, index, index, index, index) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>

    affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c256) step 1 : index {
      affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index {
        "memref.store"(%f1, %buf, %i, %j) : (f32, memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        affine.yield
      }
      affine.yield
    }

    %sum = affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c256) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index iter_args(%acc2 = %acc : f32) {
        %v = "memref.load"(%buf, %i, %j) : (memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
        %next = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        affine.yield %next : f32
      }
      affine.yield %inner : f32
    }

    "memref.store"(%sum, %out, %c0) : (f32, memref<1xf32>, index) -> ()
    "func.return"() : () -> ()
  }
}
