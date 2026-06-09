builtin.module {
  func.func @matmul_strided(
    %n_size : !d_tensor.size,
    %m_size : !d_tensor.size,
    %k_size : !d_tensor.size,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %Aflat : !d_memref.memref<[], f32>,
    %Bflat : !d_memref.memref<[], f32>,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %A = d_memref.reinterpret_cast %Aflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>

    %B = d_memref.reinterpret_cast %Bflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>

    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m_size) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
