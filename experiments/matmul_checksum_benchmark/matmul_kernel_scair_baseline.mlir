builtin.module {
  func.func @matmul_dynamic(
    %n : index,
    %m : index,
    %k : index,
    %A : memref<?x?xf32>,
    %B : memref<?x?xf32>,
    %C : memref<?x?xf32>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
        %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = "memref.load"(%A, %i, %p) : (memref<?x?xf32>, index, index) -> f32
          %b = "memref.load"(%B, %p, %j) : (memref<?x?xf32>, index, index) -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          affine.yield %next : f32
        }
        "memref.store"(%sum, %C, %i, %j) : (f32, memref<?x?xf32>, index, index) -> ()
        affine.yield
      }
      affine.yield
    }

    "func.return"() : () -> ()
  }
}
