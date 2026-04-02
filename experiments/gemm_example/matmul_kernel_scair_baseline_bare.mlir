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

    "affine.for"(%c0, %n) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %m) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        %sum = "affine.for"(%c0, %k, %f0) <{
          lowerBoundMap = affine_map<(d0) -> (d0)>,
          upperBoundMap = affine_map<(d0) -> (d0)>,
          step = 1 : index,
          operandSegmentSizes = array<i32: 1, 1, 1>
        }> ({
        ^bb0(%p: index, %acc: f32):
          %a = "memref.load"(%A, %i, %p) : (memref<?x?xf32>, index, index) -> f32
          %b = "memref.load"(%B, %p, %j) : (memref<?x?xf32>, index, index) -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          "affine.yield"(%next) : (f32) -> ()
        }) : (index, index, f32) -> f32
        "memref.store"(%sum, %C, %i, %j) : (f32, memref<?x?xf32>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    "func.return"() : () -> ()
  }
}
