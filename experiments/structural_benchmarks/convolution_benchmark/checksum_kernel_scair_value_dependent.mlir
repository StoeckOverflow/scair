builtin.module {
  func.func @checksum_dynamic(
    %n_size : !d_tensor.size,
    %cout_size : !d_tensor.size,
    %oh_size : !d_tensor.size,
    %ow_size : !d_tensor.size,
    %Yflat : !d_memref.memref<[], f32>,
    %out : !d_memref.memref<[1], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %ohow = "arith.muli"(%oh_size, %ow_size) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout_size, %ohow) : (index, index) -> index

    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size],
             f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>

    %sum_n = d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index iter_args(%acc_n = %f0 : f32) {
      %sum_co = d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout_size) step 1 : index iter_args(%acc_co = %acc_n : f32) {
        %sum_oh = d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh_size) step 1 : index iter_args(%acc_oh = %acc_co : f32) {
          %sum_ow = d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow_size) step 1 : index iter_args(%acc_ow = %acc_oh : f32) {
            %x = d_memref.load %Y[%n_idx, %co, %oh_idx, %ow_idx] : !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]> -> f32
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
