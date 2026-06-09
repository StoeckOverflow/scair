builtin.module {
  func.func @matmul_full_factorized_tiling(
    %m0 : index,
    %m1 : index,
    %n0 : index,
    %n1 : index,
    %k0 : index,
    %k1 : index,
    %Aflat : !d_memref.memref<[], f32>,
    %Bflat : !d_memref.memref<[], f32>,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %m = "arith.muli"(%m0, %m1) : (index, index) -> index
    %n = "arith.muli"(%n0, %n1) : (index, index) -> index
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32


    %A = d_memref.reinterpret_cast %Aflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%m, %k], f32,
             offset: 0, strides: [%k, %c1]>

    %B = d_memref.reinterpret_cast %Bflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%k, %n], f32,
             offset: 0, strides: [%n, %c1]>

    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%m, %n], f32,
             offset: 0, strides: [%n, %c1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%m, %k], f32, offset: 0, strides: [%k, %c1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k, %n], f32, offset: 0, strides: [%n, %c1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, %c1]>
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
