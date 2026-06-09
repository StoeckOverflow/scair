builtin.module {
  func.func @conv2d_reduction_dim_tiling(
    %n_size : !d_tensor.size, %cin0_size : !d_tensor.size, %cin1_size : !d_tensor.pos_size,
    %h_size : !d_tensor.size, %w_size : !d_tensor.size, %cout_size : !d_tensor.size,
    %kh_size : !d_tensor.pos_size, %kw_size : !d_tensor.pos_size,
    %oh_size : !d_tensor.size, %ow_size : !d_tensor.size,
    %Xflat : !d_memref.memref<[], f32>, %Kflat : !d_memref.memref<[], f32>, %Yflat : !d_memref.memref<[], f32>
  ) {
    %cin_size = "d_tensor.size.mul"(%cin0_size, %cin1_size) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %khkw_size = "d_tensor.size.mul"(%kh_size, %kw_size) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
    %red_tile_size = "d_tensor.size.mul"(%cin1_size, %khkw_size) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
    %cin_khkw_size = "d_tensor.size.mul"(%cin0_size, %red_tile_size) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %hw = "arith.muli"(%h_size, %w_size) : (index, index) -> index
    %chw = "arith.muli"(%cin_size, %hw) : (index, index) -> index
    %ohow = "arith.muli"(%oh_size, %ow_size) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout_size, %ohow) : (index, index) -> index
    %X = d_memref.reinterpret_cast %Xflat : !d_memref.memref<[], f32> to !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size], f32, offset: 0, strides: [%chw, %hw, %w_size, %c1, %w_size, %c1]>
    %K = d_memref.reinterpret_cast %Kflat : !d_memref.memref<[], f32> to !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size], f32, offset: 0, strides: [%cin_khkw_size, %khkw_size, %kw_size, %c1]>
    %Y = d_memref.reinterpret_cast %Yflat : !d_memref.memref<[], f32> to !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>
    d_affine.for %ni = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout_size) step 1 : index {
        d_affine.for %ohi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh_size) step 1 : index {
          d_affine.for %owi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow_size) step 1 : index {
            %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw_size) step 1 : index iter_args(%acc = %f0 : f32) {
              %ci = "arith.divui"(%p, %khkw_size) : (index, index) -> index
              %filter_p = "arith.remui"(%p, %khkw_size) : (index, index) -> index
              %khi = "arith.divui"(%filter_p, %kw_size) : (index, index) -> index
              %kwi = "arith.remui"(%filter_p, %kw_size) : (index, index) -> index
              %x = d_memref.load %X[%ni, %ci, %ohi, %owi, %khi, %kwi] : !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size], f32, offset: 0, strides: [%chw, %hw, %w_size, %c1, %w_size, %c1]> -> f32
              %k = d_memref.load %K[%co, %ci, %khi, %kwi] : !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size], f32, offset: 0, strides: [%cin_khkw_size, %khkw_size, %kw_size, %c1]> -> f32
              %mul = "arith.mulf"(%x, %k) : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %Y[%ni, %co, %ohi, %owi] : f32, !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>
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
