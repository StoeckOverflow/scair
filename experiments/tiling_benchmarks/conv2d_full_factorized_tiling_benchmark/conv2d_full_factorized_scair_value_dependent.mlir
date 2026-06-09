builtin.module {
  func.func @conv2d_full_factorized_tiling(
    %n0_nat : !d_tensor.nat, %n1_nat : !d_tensor.posnat,
    %cin0_nat : !d_tensor.nat, %cin1_nat : !d_tensor.posnat,
    %h_nat : !d_tensor.nat, %w_nat : !d_tensor.nat,
    %cout0_nat : !d_tensor.nat, %cout1_nat : !d_tensor.posnat,
    %kh_nat : !d_tensor.posnat, %kw_nat : !d_tensor.posnat,
    %oh0_nat : !d_tensor.nat, %oh1_nat : !d_tensor.posnat,
    %ow0_nat : !d_tensor.nat, %ow1_nat : !d_tensor.posnat,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) {
    %n_nat = "d_tensor.nat.mul"(%n0_nat, %n1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %cin_nat = "d_tensor.nat.mul"(%cin0_nat, %cin1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %cout_nat = "d_tensor.nat.mul"(%cout0_nat, %cout1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %oh_nat = "d_tensor.nat.mul"(%oh0_nat, %oh1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %ow_nat = "d_tensor.nat.mul"(%ow0_nat, %ow1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %khkw_nat = "d_tensor.nat.mul"(%kh_nat, %kw_nat) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
    %red_tile_nat = "d_tensor.nat.mul"(%cin1_nat, %khkw_nat) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
    %cin_khkw_nat = "d_tensor.nat.mul"(%cin0_nat, %red_tile_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "d_tensor.shape.to_index"(%n_nat) : (!d_tensor.nat) -> index
    %cin = "d_tensor.shape.to_index"(%cin_nat) : (!d_tensor.nat) -> index
    %h = "d_tensor.shape.to_index"(%h_nat) : (!d_tensor.nat) -> index
    %w = "d_tensor.shape.to_index"(%w_nat) : (!d_tensor.nat) -> index
    %cout = "d_tensor.shape.to_index"(%cout_nat) : (!d_tensor.nat) -> index
    %kh = "d_tensor.shape.to_index"(%kh_nat) : (!d_tensor.posnat) -> index
    %kw = "d_tensor.shape.to_index"(%kw_nat) : (!d_tensor.posnat) -> index
    %oh = "d_tensor.shape.to_index"(%oh_nat) : (!d_tensor.nat) -> index
    %ow = "d_tensor.shape.to_index"(%ow_nat) : (!d_tensor.nat) -> index
    %khkw = "d_tensor.shape.to_index"(%khkw_nat) : (!d_tensor.posnat) -> index
    %cin_khkw = "d_tensor.shape.to_index"(%cin_khkw_nat) : (!d_tensor.nat) -> index
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32,
             offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]>
    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32,
             offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]>
    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32,
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
              %x = d_memref.load %X[%ni, %ci, %ohi, %owi, %khi, %kwi] : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]> -> f32
              %k = d_memref.load %K[%co, %ci, %khi, %kwi] : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]> -> f32
              %mul = "arith.mulf"(%x, %k) : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %Y[%ni, %co, %ohi, %owi] : f32, !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
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
