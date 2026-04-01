module {
  func.func @matmul_dynamic(
      %n : index, %m : index, %k : index,
      %A : memref<?x?xf32>,
      %B : memref<?x?xf32>,
      %C : memref<?x?xf32>) attributes {llvm.emit_c_interface} {

    affine.for %i = 0 to %n {
      affine.for %j = 0 to %m {
        %init = arith.constant 0.0 : f32
        %sum = affine.for %p = 0 to %k iter_args(%acc = %init) -> f32 {
          %a = affine.load %A[%i, %p] : memref<?x?xf32>
          %b = affine.load %B[%p, %j] : memref<?x?xf32>
          %mul = arith.mulf %a, %b : f32
          %add = arith.addf %acc, %mul : f32
          affine.yield %add : f32
        }
        affine.store %sum, %C[%i, %j] : memref<?x?xf32>
      }
    }

    return
  }
}