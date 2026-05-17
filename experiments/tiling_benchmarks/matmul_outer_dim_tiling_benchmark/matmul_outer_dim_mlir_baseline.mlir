module {
  func.func @matmul_outer_dim_tiling(
      %m0 : index,
      %m1 : index,
      %n0 : index,
      %n1 : index,
      %k : index,
      %Aflat : memref<?xf32>,
      %Bflat : memref<?xf32>,
      %Cflat : memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %f0 = arith.constant 0.0 : f32
    %m = arith.muli %m0, %m1 : index
    %n = arith.muli %n0, %n1 : index

    %A = memref.reinterpret_cast %Aflat to
      offset: [%c0],
      sizes: [%m, %k],
      strides: [%k, %c1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>

    %B = memref.reinterpret_cast %Bflat to
      offset: [%c0],
      sizes: [%k, %n],
      strides: [%n, %c1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>

    %C = memref.reinterpret_cast %Cflat to
      offset: [%c0],
      sizes: [%m, %n],
      strides: [%n, %c1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>

    affine.for %i = 0 to %m {
      affine.for %j = 0 to %n {
        %sum = affine.for %p = 0 to %k iter_args(%acc = %f0) -> f32 {
          %a = memref.load %A[%i, %p] : memref<?x?xf32, strided<[?, ?], offset: ?>>
          %b = memref.load %B[%p, %j] : memref<?x?xf32, strided<[?, ?], offset: ?>>
          %mul = arith.mulf %a, %b : f32
          %next = arith.addf %acc, %mul : f32
          affine.yield %next : f32
        }
        memref.store %sum, %C[%i, %j] : memref<?x?xf32, strided<[?, ?], offset: ?>>
      }
    }
    return
  }
}
