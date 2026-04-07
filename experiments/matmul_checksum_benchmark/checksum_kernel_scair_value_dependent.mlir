builtin.module {
  func.func @checksum_dynamic(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %out_nat : !dtensor.nat,
    %C : !d_memref.memref<[%n_nat, %m_nat], f32>,
    %out : !d_memref.memref<[%out_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index iter_args(%acc2 = %acc : f32) {
        %x = d_memref.load %C[%i, %j] : !d_memref.memref<[%n_nat, %m_nat], f32> -> f32
        %next = "arith.addf"(%acc2, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %next : (f32)
      }
      d_affine.yield %inner : (f32)
    }

    d_memref.store %sum, %out[%c0] : f32, !d_memref.memref<[%out_nat], f32>
    "func.return"() : () -> ()
  }
}
