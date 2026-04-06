module {
  func.func @checksum_dynamic(
      %n : index, %m : index,
      %C : memref<?x?xf32>,
      %out : memref<1xf32>) attributes {llvm.emit_c_interface} {

    %f0 = arith.constant 0.0 : f32
    %c0 = arith.constant 0 : index

    %sum = affine.for %i = 0 to %n iter_args(%acc = %f0) -> f32 {
      %inner = affine.for %j = 0 to %m iter_args(%acc2 = %acc) -> f32 {
        %x = affine.load %C[%i, %j] : memref<?x?xf32>
        %y = arith.addf %acc2, %x : f32
        affine.yield %y : f32
      }
      affine.yield %inner : f32
    }

    affine.store %sum, %out[%c0] : memref<1xf32>
    return
  }
}