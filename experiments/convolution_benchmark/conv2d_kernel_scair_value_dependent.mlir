builtin.module {
  func.func @conv2d_dynamic(
    %n_nat : !dtensor.nat,
    %cin_nat : !dtensor.nat,
    %h_nat : !dtensor.nat,
    %w_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %kh_nat : !dtensor.nat,
    %kw_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %xflat_nat : !dtensor.nat,
    %kflat_nat : !dtensor.nat,
    %yflat_nat : !dtensor.nat,
    %Xflat : !d_memref.memref<[%xflat_nat], f32>,
    %Kflat : !d_memref.memref<[%kflat_nat], f32>,
    %Yflat : !d_memref.memref<[%yflat_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %cin = "dtensor.shape.to_index"(%cin_nat) : (!dtensor.nat) -> index
    %h = "dtensor.shape.to_index"(%h_nat) : (!dtensor.nat) -> index
    %w = "dtensor.shape.to_index"(%w_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %kh = "dtensor.shape.to_index"(%kh_nat) : (!dtensor.nat) -> index
    %kw = "dtensor.shape.to_index"(%kw_nat) : (!dtensor.nat) -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index

    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[%xflat_nat], f32>
        to !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32, offset: %c0, strides: [%chw, %hw, %w, %c1, %w, %c1]>
    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[%kflat_nat], f32>
        to !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32, offset: %c0, strides: [%cin_khkw, %khkw, %kw, %c1]>
    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[%yflat_nat], f32>
        to !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: %c0, strides: [%cout_ohow, %ohow, %ow, %c1]>

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum_ci = d_affine.for %ci = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin) step 1 : index iter_args(%acc_ci = %f0 : f32) {
              %sum_kh = d_affine.for %kh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kh) step 1 : index iter_args(%acc_kh = %acc_ci : f32) {
                %sum_kw = d_affine.for %kw_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kw) step 1 : index iter_args(%acc_kw = %acc_kh : f32) {
                  %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32, offset: %c0, strides: [%chw, %hw, %w, %c1, %w, %c1]> -> f32
                  %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32, offset: %c0, strides: [%cin_khkw, %khkw, %kw, %c1]> -> f32
                  %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  %next = "arith.addf"(%acc_kw, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  d_affine.yield %next : (f32)
                }
                d_affine.yield %sum_kw : (f32)
              }
              d_affine.yield %sum_kh : (f32)
            }
            d_memref.store %sum_ci, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: %c0, strides: [%cout_ohow, %ohow, %ow, %c1]>
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
