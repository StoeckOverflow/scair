builtin.module {
  func.func @semi_affine_fill_and_sum(
    %rows_nat : !d_tensor.nat,
    %cols_nat : !d_tensor.nat,
    %stride0 : index,
    %stride1 : index,
    %flat : !d_memref.memref<[], f32>,
    %out : !d_memref.memref<[1], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %rows = "d_tensor.shape.to_index"(%rows_nat) : (!d_tensor.nat) -> index
    %cols = "d_tensor.shape.to_index"(%cols_nat) : (!d_tensor.nat) -> index

    %buf = d_memref.reinterpret_cast %flat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%rows_nat, %cols_nat], f32, offset: 0, strides: [%stride0, %stride1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%rows) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cols) step 1 : index {
        d_memref.store %f1, %buf[%i, %j] : f32, !d_memref.memref<[%rows_nat, %cols_nat], f32, offset: 0, strides: [%stride0, %stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%rows) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cols) step 1 : index iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%rows_nat, %cols_nat], f32, offset: 0, strides: [%stride0, %stride1]> -> f32
        %next = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %next : (f32)
      }
      d_affine.yield %inner : (f32)
    }

    d_memref.store %sum, %out[%c0] : f32, !d_memref.memref<[1], f32>
    "func.return"() : () -> ()
  }
}
