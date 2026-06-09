builtin.module {
  func.func @conv2d_dynamic(
    %n_size : !d_tensor.size,
    %cin_size : !d_tensor.size,
    %h_size : !d_tensor.size,
    %w_size : !d_tensor.size,
    %cout_size : !d_tensor.size,
    %kh_size : !d_tensor.size,
    %kw_size : !d_tensor.size,
    %oh_size : !d_tensor.size,
    %ow_size : !d_tensor.size,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %hw = "d_tensor.size.mul"(%h_size, %w_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %chw = "d_tensor.size.mul"(%cin_size, %hw) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %khkw = "d_tensor.size.mul"(%kh_size, %kw_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %cin_khkw = "d_tensor.size.mul"(%cin_size, %khkw) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %ohow = "d_tensor.size.mul"(%oh_size, %ow_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %cout_ohow = "d_tensor.size.mul"(%cout_size, %ohow) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size],
             f32, offset: 0, strides: [%chw, %hw, %w_size, %c1, %w_size, %c1]>

    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size],
             f32, offset: 0, strides: [%cin_khkw, %khkw, %kw_size, %c1]>

    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size],
             f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>

    "func.call"(%n_size, %cin_size, %h_size, %w_size, %cout_size, %kh_size, %kw_size, %oh_size, %ow_size,
                 %chw, %hw, %w_size, %c1, %cin_khkw, %khkw, %kw_size, %cout_ohow, %ohow, %ow_size,
                 %X, %K, %Y)
      <{callee = @conv2d_dynamic_typed}>
      : (!d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size,
         !d_tensor.size, !d_tensor.size, !d_tensor.size, index, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size, !d_tensor.size,
         !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size], f32, offset: 0, strides: [%chw, %hw, %w_size, %c1, %w_size, %c1]>,
         !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw_size, %c1]>,
         !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>) -> ()
    "func.return"() : () -> ()
  }

  func.func @conv2d_dynamic_typed(
    %n_size : !d_tensor.size,
    %cin_size : !d_tensor.size,
    %h_size : !d_tensor.size,
    %w_size : !d_tensor.size,
    %cout_size : !d_tensor.size,
    %kh_size : !d_tensor.size,
    %kw_size : !d_tensor.size,
    %oh_size : !d_tensor.size,
    %ow_size : !d_tensor.size,
    %x_stride0 : !d_tensor.size,
    %x_stride1 : !d_tensor.size,
    %x_stride2 : !d_tensor.size,
    %unit_stride : index,
    %k_stride0 : !d_tensor.size,
    %k_stride1 : !d_tensor.size,
    %k_stride2 : !d_tensor.size,
    %y_stride0 : !d_tensor.size,
    %y_stride1 : !d_tensor.size,
    %y_stride2 : !d_tensor.size,
    %X : !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size],
         f32, offset: 0, strides: [%x_stride0, %x_stride1, %x_stride2, %unit_stride, %x_stride2, %unit_stride]>,
    %K : !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size],
         f32, offset: 0, strides: [%k_stride0, %k_stride1, %k_stride2, %unit_stride]>,
    %Y : !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size],
         f32, offset: 0, strides: [%y_stride0, %y_stride1, %y_stride2, %unit_stride]>
  ) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout_size) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh_size) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow_size) step 1 : index {
            %sum_ci = d_affine.for %ci = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_size) step 1 : index iter_args(%acc_ci = %f0 : f32) {
              %sum_kh = d_affine.for %kh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kh_size) step 1 : index iter_args(%acc_kh = %acc_ci : f32) {
                %sum_kw = d_affine.for %kw_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kw_size) step 1 : index iter_args(%acc_kw = %acc_kh : f32) {
                  %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size], f32, offset: 0, strides: [%x_stride0, %x_stride1, %x_stride2, %unit_stride, %x_stride2, %unit_stride]> -> f32
                  %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size], f32, offset: 0, strides: [%k_stride0, %k_stride1, %k_stride2, %unit_stride]> -> f32
                  %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  %next = "arith.addf"(%acc_kw, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  d_affine.yield %next : (f32)
                }
                d_affine.yield %sum_kw : (f32)
              }
              d_affine.yield %sum_kh : (f32)
            }
            d_memref.store %sum_ci, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size], f32, offset: 0, strides: [%y_stride0, %y_stride1, %y_stride2, %unit_stride]>
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
