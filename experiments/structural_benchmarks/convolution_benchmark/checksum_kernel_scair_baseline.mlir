builtin.module {
  func.func @checksum_dynamic(
    %n : index,
    %cout : index,
    %oh : index,
    %ow : index,
    %Yflat : memref<?xf32>,
    %out : memref<1xf32>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %Y = "memref.reinterpret_cast"(%Yflat, %c0, %n, %cout, %oh, %ow, %cout_ohow, %ohow, %ow, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, 1], offset: 0>>

    %sum_n = affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc_n = %f0 : f32) {
      %sum_co = affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index iter_args(%acc_co = %acc_n : f32) {
        %sum_oh = affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index iter_args(%acc_oh = %acc_co : f32) {
          %sum_ow = affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index iter_args(%acc_ow = %acc_oh : f32) {
            %x = "memref.load"(%Y, %n_idx, %co, %oh_idx, %ow_idx) : (memref<?x?x?x?xf32, strided<[?, ?, ?, 1], offset: 0>>, index, index, index, index) -> f32
            %next = "arith.addf"(%acc_ow, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            affine.yield %next : f32
          }
          affine.yield %sum_ow : f32
        }
        affine.yield %sum_oh : f32
      }
      affine.yield %sum_co : f32
    }

    "memref.store"(%sum_n, %out, %c0) : (f32, memref<1xf32>, index) -> ()
    "func.return"() : () -> ()
  }
}
