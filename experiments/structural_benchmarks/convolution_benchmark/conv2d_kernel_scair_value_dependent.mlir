builtin.module {
  func.func @conv2d_dynamic(
    %n : index,
    %cin : index,
    %h : index,
    %w : index,
    %cout : index,
    %kh : index,
    %kw : index,
    %oh : index,
    %ow : index,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw],
             f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]>

    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%cout, %cin, %kh, %kw],
             f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]>

    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n, %cout, %oh, %ow],
             f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>

    "func.call"(%n, %cin, %h, %w, %cout, %kh, %kw, %oh, %ow,
                 %chw, %hw, %w, %c1, %cin_khkw, %khkw, %kw, %cout_ohow, %ohow, %ow,
                 %X, %K, %Y)
      <{callee = @conv2d_dynamic_typed}>
      : (index, index, index, index, index, index, index, index, index,
         index, index, index, index, index, index, index, index, index, index,
         !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw], f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]>,
         !d_memref.memref<[%cout, %cin, %kh, %kw], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]>,
         !d_memref.memref<[%n, %cout, %oh, %ow], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>) -> ()
    "func.return"() : () -> ()
  }

  func.func @conv2d_dynamic_typed(
    %n : index,
    %cin : index,
    %h : index,
    %w : index,
    %cout : index,
    %kh : index,
    %kw : index,
    %oh : index,
    %ow : index,
    %x_stride0 : index,
    %x_stride1 : index,
    %x_stride2 : index,
    %unit_stride : index,
    %k_stride0 : index,
    %k_stride1 : index,
    %k_stride2 : index,
    %y_stride0 : index,
    %y_stride1 : index,
    %y_stride2 : index,
    %X : !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw],
         f32, offset: 0, strides: [%x_stride0, %x_stride1, %x_stride2, %unit_stride, %x_stride2, %unit_stride]>,
    %K : !d_memref.memref<[%cout, %cin, %kh, %kw],
         f32, offset: 0, strides: [%k_stride0, %k_stride1, %k_stride2, %unit_stride]>,
    %Y : !d_memref.memref<[%n, %cout, %oh, %ow],
         f32, offset: 0, strides: [%y_stride0, %y_stride1, %y_stride2, %unit_stride]>
  ) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum_ci = d_affine.for %ci = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin) step 1 : index iter_args(%acc_ci = %f0 : f32) {
              %sum_kh = d_affine.for %kh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kh) step 1 : index iter_args(%acc_kh = %acc_ci : f32) {
                %sum_kw = d_affine.for %kw_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kw) step 1 : index iter_args(%acc_kw = %acc_kh : f32) {
                  %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw], f32, offset: 0, strides: [%x_stride0, %x_stride1, %x_stride2, %unit_stride, %x_stride2, %unit_stride]> -> f32
                  %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout, %cin, %kh, %kw], f32, offset: 0, strides: [%k_stride0, %k_stride1, %k_stride2, %unit_stride]> -> f32
                  %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  %next = "arith.addf"(%acc_kw, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  d_affine.yield %next : (f32)
                }
                d_affine.yield %sum_kw : (f32)
              }
              d_affine.yield %sum_kh : (f32)
            }
            d_memref.store %sum_ci, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n, %cout, %oh, %ow], f32, offset: 0, strides: [%y_stride0, %y_stride1, %y_stride2, %unit_stride]>
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
