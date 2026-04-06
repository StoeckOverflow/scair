builtin.module {
  func.func @control_flow_selected_subview_reduction(
    %sel : i1,
    %flat : memref<?xf32>,
    %out : memref<1xf32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c2 = "arith.constant"() <{value = 2 : index}> : () -> index
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    // Phase-3/4 executable realization for the refined route:
    // carry the selected layout metadata explicitly through control flow and
    // loop state, then rebuild the derived view from that metadata on demand.
    %stride = "scf.if"(%sel) ({
      "scf.yield"(%c1) : (index) -> ()
    }, {
      "scf.yield"(%c2) : (index) -> ()
    }) : (i1) -> index

    %carry_stride, %sum = "affine.for"(%c0, %c8, %stride, %f0) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 2>
    }> ({
    ^bb0(%i: index, %cur_stride: index, %acc: f32):
      %cur = "memref.reinterpret_cast"(%flat, %c0, %c8, %cur_stride)
        <{operandSegmentSizes = array<i32: 1, 1, 1, 1>}>
        : (memref<?xf32>, index, index, index)
          -> memref<8xf32, strided<[?], offset: ?>>
      %v = "memref.load"(%cur, %i)
        : (memref<8xf32, strided<[?], offset: ?>>, index) -> f32
      %next = "arith.addf"(%acc, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
      "affine.yield"(%cur_stride, %next) : (index, f32) -> ()
    }) : (index, index, index, f32) -> (index, f32)

    "memref.store"(%sum, %out, %c0) : (f32, memref<1xf32>, index) -> ()
    "func.return"() : () -> ()
  }
}
