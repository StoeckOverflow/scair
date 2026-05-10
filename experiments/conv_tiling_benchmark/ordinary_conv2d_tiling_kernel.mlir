builtin.module {
  func.func @conv2d_tiling_dynamic(
    %n : index,
    %cin : index,
    %h : index,
    %w : index,
    %cout : index,
    %kh : index,
    %kw : index,
    %oh : index,
    %ow : index,
    %Xflat : memref<?xf32>,
    %Kflat : memref<?xf32>,
    %Yflat : memref<?xf32>
  ) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = "memref.reinterpret_cast"(%Xflat, %c0, %n, %cin, %oh, %ow, %kh, %kw, %chw, %hw, %w, %c1, %w, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 6, 6>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, 1], offset: 0>>
    %K = "memref.reinterpret_cast"(%Kflat, %c0, %cout, %cin, %kh, %kw, %cin_khkw, %khkw, %kw, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, 1], offset: 0>>
    %Y = "memref.reinterpret_cast"(%Yflat, %c0, %n, %cout, %oh, %ow, %cout_ohow, %ohow, %ow, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, 1], offset: 0>>

    affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw) step 1 : index iter_args(%acc = %f0 : f32) {
              %ci = "arith.divui"(%p, %khkw) : (index, index) -> index
              %filter_p = "arith.remui"(%p, %khkw) : (index, index) -> index
              %kh_idx = "arith.divui"(%filter_p, %kw) : (index, index) -> index
              %kw_idx = "arith.remui"(%filter_p, %kw) : (index, index) -> index
              %x = "memref.load"(%X, %n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx) : (memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, 1], offset: 0>>, index, index, index, index, index, index) -> f32
              %k = "memref.load"(%K, %co, %ci, %kh_idx, %kw_idx) : (memref<?x?x?x?xf32, strided<[?, ?, ?, 1], offset: 0>>, index, index, index, index) -> f32
              %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              affine.yield %next : f32
            }
            "memref.store"(%sum, %Y, %n_idx, %co, %oh_idx, %ow_idx) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, 1], offset: 0>>, index, index, index, index) -> ()
            affine.yield
          }
          affine.yield
        }
        affine.yield
      }
      affine.yield
    }

    "func.return"() : () -> ()
  }
}
