builtin.module {
  func.func @control_flow_selected_subview_reduction(
    %sel : i1,
    %flat : memref<?xf32>,
    %out : memref<1xf32>
  ) attributes {llvm.emit_c_interface} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c2 = "arith.constant"() <{value = 2 : index}> : () -> index
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %view0 = "memref.reinterpret_cast"(%flat, %c0, %c8, %c1)
      <{
        operandSegmentSizes = array<i32: 1, 1, 1, 1>,
        static_offsets = array<i64: -9223372036854775808>,
        static_sizes = array<i64: -9223372036854775808>,
        static_strides = array<i64: -9223372036854775808>
      }>
      : (memref<?xf32>, index, index, index)
        -> memref<?xf32, strided<[?], offset: ?>>

    %view1 = "memref.reinterpret_cast"(%flat, %c0, %c8, %c2)
      <{
        operandSegmentSizes = array<i32: 1, 1, 1, 1>,
        static_offsets = array<i64: -9223372036854775808>,
        static_sizes = array<i64: -9223372036854775808>,
        static_strides = array<i64: -9223372036854775808>
      }>
      : (memref<?xf32>, index, index, index)
        -> memref<?xf32, strided<[?], offset: ?>>

    %view = scf.if %sel -> (memref<?xf32, strided<[?], offset: ?>>) {
      scf.yield %view0 : memref<?xf32, strided<[?], offset: ?>>
    } else {
      scf.yield %view1 : memref<?xf32, strided<[?], offset: ?>>
    }

    %carry_view, %sum = affine.for %i = 0 to 8
        iter_args(%cur = %view, %acc = %f0)
        -> (memref<?xf32, strided<[?], offset: ?>>, f32) {
      %v = memref.load %cur[%i] : memref<?xf32, strided<[?], offset: ?>>
      %next = arith.addf %acc, %v : f32
      affine.yield %cur, %next : memref<?xf32, strided<[?], offset: ?>>, f32
    }

    memref.store %sum, %out[%c0] : memref<1xf32>
    return
  }
}
