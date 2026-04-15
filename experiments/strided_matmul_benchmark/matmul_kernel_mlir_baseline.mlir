builtin.module {
  func.func @matmul_strided(
      %n : index,
      %m : index,
      %k : index,
      %a_stride0 : index,
      %a_stride1 : index,
      %b_stride0 : index,
      %b_stride1 : index,
      %c_stride0 : index,
      %c_stride1 : index,
      %Aflat : memref<?xf32>,
      %Bflat : memref<?xf32>,
      %Cflat : memref<?xf32>) attributes {llvm.emit_c_interface} {
    %f0 = arith.constant 0.0 : f32

    %A = memref.reinterpret_cast %Aflat to
      offset: [0],
      sizes: [%n, %k],
      strides: [%a_stride0, %a_stride1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>

    %B = memref.reinterpret_cast %Bflat to
      offset: [0],
      sizes: [%k, %m],
      strides: [%b_stride0, %b_stride1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>

    %C = memref.reinterpret_cast %Cflat to
      offset: [0],
      sizes: [%n, %m],
      strides: [%c_stride0, %c_stride1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>

    affine.for %i = 0 to %n {
      affine.for %j = 0 to %m {
        %sum = affine.for %p = 0 to %k iter_args(%acc = %f0) -> f32 {
          %a = memref.load %A[%i, %p] : memref<?x?xf32, strided<[?, ?], offset: 0>>
          %b = memref.load %B[%p, %j] : memref<?x?xf32, strided<[?, ?], offset: 0>>
          %mul = arith.mulf %a, %b : f32
          %next = arith.addf %acc, %mul : f32
          affine.yield %next : f32
        }
        memref.store %sum, %C[%i, %j] : memref<?x?xf32, strided<[?, ?], offset: 0>>
        affine.yield
      }
    }
    return
  }
}
