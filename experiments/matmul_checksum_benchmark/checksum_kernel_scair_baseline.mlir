builtin.module {
  func.func @checksum_dynamic(
    %n : index,
    %m : index,
    %C : memref<?x?xf32>,
    %out : memref<1xf32>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %sum = affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index iter_args(%acc2 = %acc : f32) {
        %x = "memref.load"(%C, %i, %j) : (memref<?x?xf32>, index, index) -> f32
        %next = "arith.addf"(%acc2, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        affine.yield %next : f32
      }
      affine.yield %inner : f32
    }

    "memref.store"(%sum, %out, %c0) : (f32, memref<1xf32>, index) -> ()
    "func.return"() : () -> ()
  }
}
