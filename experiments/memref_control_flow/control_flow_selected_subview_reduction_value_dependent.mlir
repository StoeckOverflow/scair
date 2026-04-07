builtin.module {
  func.func @control_flow_selected_subview_reduction(
    %sel : i1,
    %flat_nat : !dtensor.nat,
    %out_nat : !dtensor.nat,
    %flat : !d_memref.memref<[%flat_nat], f32>,
    %out : !d_memref.memref<[%out_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c2 = "arith.constant"() <{value = 2 : index}> : () -> index
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %view_n = "dtensor.index_to_nat"(%c8) : (index) -> !dtensor.nat

    %stride = "scf.if"(%sel) ({
      "scf.yield"(%c1) : (index) -> ()
    }, {
      "scf.yield"(%c2) : (index) -> ()
    }) : (i1) -> index

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c8) step 1 : index iter_args(%acc = %f0 : f32) {
      %cur = d_memref.reinterpret_cast %flat
        : !d_memref.memref<[%flat_nat], f32>
          to !d_memref.memref<[%view_n], f32, offset: %c0, strides: [%stride]>
      %v = d_memref.load %cur[%i]
        : !d_memref.memref<[%view_n], f32, offset: %c0, strides: [%stride]> -> f32
      %next = "arith.addf"(%acc, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
      d_affine.yield %next : (f32)
    }

    d_memref.store %sum, %out[%c0] : f32, !d_memref.memref<[%out_nat], f32>
    "func.return"() : () -> ()
  }
}
