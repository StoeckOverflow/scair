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
    %view0 = memref.reinterpret_cast %flat to
      offset: [%c0],
      sizes: [%c8],
      strides: [%c1]
    : memref<?xf32> to memref<8xf32, strided<[?], offset: ?>>

    %view1 = memref.reinterpret_cast %flat to
      offset: [%c0],
      sizes: [%c8],
      strides: [%c2]
    : memref<?xf32> to memref<8xf32, strided<[?], offset: ?>>

    %view = "scf.if"(%sel) ({
      "scf.yield"(%view0) : (memref<8xf32, strided<[?], offset: ?>>) -> ()
    }, {
      "scf.yield"(%view1) : (memref<8xf32, strided<[?], offset: ?>>) -> ()
    }) : (i1) -> memref<8xf32, strided<[?], offset: ?>>

    %carry_view, %sum = affine.for %i = 0 to 8
        iter_args(%cur = %view, %acc = %f0)
        -> (memref<8xf32, strided<[?], offset: ?>>, f32) {
      %v = "memref.load"(%cur, %i)
        : (memref<8xf32, strided<[?], offset: ?>>, index) -> f32
      %next = "arith.addf"(%acc, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
      "affine.yield"(%cur, %next) : (memref<8xf32, strided<[?], offset: ?>>, f32) -> ()
    }

    "memref.store"(%sum, %out, %c0) : (f32, memref<1xf32>, index) -> ()
    "func.return"() : () -> ()
  }
}
