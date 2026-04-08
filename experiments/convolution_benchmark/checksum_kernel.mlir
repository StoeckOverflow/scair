module {
  func.func @checksum_dynamic(
      %n : index, %cout : index, %oh : index, %ow : index,
      %Yflat : memref<?xf32>,
      %out : memref<1xf32>) attributes {llvm.emit_c_interface} {

    %f0 = arith.constant 0.0 : f32
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %ohow = arith.muli %oh, %ow : index
    %cout_ohow = arith.muli %cout, %ohow : index

    %Y = memref.reinterpret_cast %Yflat to
      offset: [%c0],
      sizes: [%n, %cout, %oh, %ow],
      strides: [%cout_ohow, %ohow, %ow, %c1]
      : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    %sum_n = affine.for %n_idx = 0 to %n iter_args(%acc_n = %f0) -> f32 {
      %sum_co = affine.for %co = 0 to %cout iter_args(%acc_co = %acc_n) -> f32 {
        %sum_oh = affine.for %oh_idx = 0 to %oh iter_args(%acc_oh = %acc_co) -> f32 {
          %sum_ow = affine.for %ow_idx = 0 to %ow iter_args(%acc_ow = %acc_oh) -> f32 {
            %x = memref.load %Y[%n_idx, %co, %oh_idx, %ow_idx] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
            %next = arith.addf %acc_ow, %x : f32
            affine.yield %next : f32
          }
          affine.yield %sum_ow : f32
        }
        affine.yield %sum_oh : f32
      }
      affine.yield %sum_co : f32
    }

    affine.store %sum_n, %out[%c0] : memref<1xf32>
    return
  }
}
