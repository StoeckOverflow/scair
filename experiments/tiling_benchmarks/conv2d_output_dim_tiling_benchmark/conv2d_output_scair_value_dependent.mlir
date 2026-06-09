builtin.module {
  func.func @conv2d_output_dim_tiling(
    %n0 : index, %n1 : index,
    %cin : index,
    %h : index, %w : index,
    %cout0 : index, %cout1 : index,
    %kh : index, %kw : index,
    %oh0 : index, %oh1 : index,
    %ow0 : index, %ow1 : index,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) {
    %n = "arith.muli"(%n0, %n1) : (index, index) -> index
    %cout = "arith.muli"(%cout0, %cout1) : (index, index) -> index
    %oh = "arith.muli"(%oh0, %oh1) : (index, index) -> index
    %ow = "arith.muli"(%ow0, %ow1) : (index, index) -> index
    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw], f32,
             offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]>
    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%cout, %cin, %kh, %kw], f32,
             offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]>
    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n, %cout, %oh, %ow], f32,
             offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>

    d_affine.for %ni = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        d_affine.for %ohi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          d_affine.for %owi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw) step 1 : index iter_args(%acc = %f0 : f32) {
              %ci = "arith.divui"(%p, %khkw) : (index, index) -> index
              %filter_p = "arith.remui"(%p, %khkw) : (index, index) -> index
              %khi = "arith.divui"(%filter_p, %kw) : (index, index) -> index
              %kwi = "arith.remui"(%filter_p, %kw) : (index, index) -> index
              %x = d_memref.load %X[%ni, %ci, %ohi, %owi, %khi, %kwi] : !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw], f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]> -> f32
              %k = d_memref.load %K[%co, %ci, %khi, %kwi] : !d_memref.memref<[%cout, %cin, %kh, %kw], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]> -> f32
              %mul = "arith.mulf"(%x, %k) : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %Y[%ni, %co, %ohi, %owi] : f32, !d_memref.memref<[%n, %cout, %oh, %ow], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
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
