builtin.module {
  func.func @checksum_dynamic(
    %n_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %Y : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>,
    %out : !d_memref.memref<[1], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index

    %sum_n = d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc_n = %f0 : f32) {
      %sum_co = d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index iter_args(%acc_co = %acc_n : f32) {
        %sum_oh = d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index iter_args(%acc_oh = %acc_co : f32) {
          %sum_ow = d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index iter_args(%acc_ow = %acc_oh : f32) {
            %x = d_memref.load %Y[%n_idx, %co, %oh_idx, %ow_idx] : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32> -> f32
            %next = "arith.addf"(%acc_ow, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            d_affine.yield %next : (f32)
          }
          d_affine.yield %sum_ow : (f32)
        }
        d_affine.yield %sum_oh : (f32)
      }
      d_affine.yield %sum_co : (f32)
    }

    d_memref.store %sum_n, %out[%c0] : f32, !d_memref.memref<[1], f32>
    "func.return"() : () -> ()
  }
}
