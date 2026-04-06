builtin.module {
  func.func @control_flow_selected_subview_reduction(
    %sel : i1,
    %flat : memref<?xf32>,
    %out : memref<1xf32>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c2 = "arith.constant"() <{value = 2 : index}> : () -> index
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    // Phase-3/4 executable realization:
    // materialize two metadata-bearing views, choose one as a memref value via
    // scf.if, then carry that selected view through the reduction loop.
    %view0 = "memref.reinterpret_cast"(%flat, %c0, %c8, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 1, 1>}>
      : (memref<?xf32>, index, index, index)
        -> memref<8xf32, strided<[?], offset: ?>>

    %view1 = "memref.reinterpret_cast"(%flat, %c0, %c8, %c2)
      <{operandSegmentSizes = array<i32: 1, 1, 1, 1>}>
      : (memref<?xf32>, index, index, index)
        -> memref<8xf32, strided<[?], offset: ?>>

    %view = "scf.if"(%sel) ({
      "scf.yield"(%view0) : (memref<8xf32, strided<[?], offset: ?>>) -> ()
    }, {
      "scf.yield"(%view1) : (memref<8xf32, strided<[?], offset: ?>>) -> ()
    }) : (i1) -> memref<8xf32, strided<[?], offset: ?>>

    %carry_view, %sum = "affine.for"(%c0, %c8, %view, %f0) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 2>
    }> ({
    ^bb0(%i: index, %cur: memref<8xf32, strided<[?], offset: ?>>, %acc: f32):
      %v = "memref.load"(%cur, %i)
        : (memref<8xf32, strided<[?], offset: ?>>, index) -> f32
      %next = "arith.addf"(%acc, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
      "affine.yield"(%cur, %next) : (memref<8xf32, strided<[?], offset: ?>>, f32) -> ()
    }) : (index, index, memref<8xf32, strided<[?], offset: ?>>, f32)
      -> (memref<8xf32, strided<[?], offset: ?>>, f32)

    "memref.store"(%sum, %out, %c0) : (f32, memref<1xf32>, index) -> ()
    "func.return"() : () -> ()
  }
}
