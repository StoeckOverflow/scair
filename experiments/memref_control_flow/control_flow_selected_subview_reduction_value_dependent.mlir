builtin.module {
  func.func @control_flow_selected_subview_reduction(
    %sel : i1,
    %flat : !d_memref.memref<[20], f32>,
    %out : !d_memref.memref<[1], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c2 = "arith.constant"() <{value = 2 : index}> : () -> index
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %view_n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %view_i = "dtensor.shape.to_index"(%view_n) : (!dtensor.nat) -> index

    %stride = "scf.if"(%sel) ({
      "scf.yield"(%c1) : (index) -> ()
    }, {
      "scf.yield"(%c2) : (index) -> ()
    }) : (i1) -> index

    %view = d_memref.subview %flat[%c0][%view_i][%stride] : !d_memref.memref<[20], f32> -> !d_memref.memref<[%view_n], f32>

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c8) step 1 : index iter_args(%acc = %f0 : f32) {
      %v = d_memref.load %view[%i] : !d_memref.memref<[%view_n], f32> -> f32
      %next = "arith.addf"(%acc, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
      d_affine.yield %next : (f32)
    }

    d_memref.store %sum, %out[%c0] : f32, !d_memref.memref<[1], f32>
    "func.return"() : () -> ()
  }
}
