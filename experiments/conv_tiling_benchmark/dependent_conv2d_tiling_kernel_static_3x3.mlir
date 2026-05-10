builtin.module {
  func.func @conv2d_tiling_static_3x3(
    %n_nat : !dtensor.nat,
    %cin_nat : !dtensor.nat,
    %h_nat : !dtensor.nat,
    %w_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %Xflat : memref<?xf32>,
    %Kflat : memref<?xf32>,
    %Yflat : memref<?xf32>
  ) {
    %khkw_nat = "dtensor.nat.const"() <{value = 9 : i32}> : () -> !dtensor.nat
    %cin_khkw_nat = "dtensor.nat.mul"(%cin_nat, %khkw_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c3 = "arith.constant"() <{value = 3 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %cin = "dtensor.shape.to_index"(%cin_nat) : (!dtensor.nat) -> index
    %h = "dtensor.shape.to_index"(%h_nat) : (!dtensor.nat) -> index
    %w = "dtensor.shape.to_index"(%w_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index
    %cin_khkw = "dtensor.shape.to_index"(%cin_khkw_nat) : (!dtensor.nat) -> index
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = "memref.reinterpret_cast"(%Xflat, %c0, %n, %cin, %oh, %ow, %c3, %c3, %chw, %hw, %w, %c1, %w, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 6, 6>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>

    %K = "memref.reinterpret_cast"(%Kflat, %c0, %cout, %cin, %c3, %c3, %cin_khkw, %c3, %c3, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    %Y = "memref.reinterpret_cast"(%Yflat, %c0, %n, %cout, %oh, %ow, %cout_ohow, %ohow, %ow, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw) step 1 : index iter_args(%acc = %f0 : f32) {
              %ci = "arith.divui"(%p, %c3) : (index, index) -> index
              %ci_true = "arith.divui"(%ci, %c3) : (index, index) -> index
              %filter_p = "arith.remui"(%p, %c3) : (index, index) -> index
              %kh_idx = "arith.remui"(%ci, %c3) : (index, index) -> index
              %kw_idx = "arith.remui"(%filter_p, %c3) : (index, index) -> index
              %x = "memref.load"(%X, %n_idx, %ci_true, %oh_idx, %ow_idx, %kh_idx, %kw_idx) : (memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>, index, index, index, index, index, index) -> f32
              %k = "memref.load"(%K, %co, %ci_true, %kh_idx, %kw_idx) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
              %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            "memref.store"(%sum, %Y, %n_idx, %co, %oh_idx, %ow_idx) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
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
