builtin.module {
  func.func @conv2d_output_dim_tiling(
    %n0 : index, %n1 : index,
    %cin : index,
    %h : index, %w : index,
    %cout0 : index, %cout1 : index,
    %kh : index, %kw : index,
    %oh0 : index, %oh1 : index,
    %ow0 : index, %ow1 : index,
    %Xflat : memref<?xf32>,
    %Kflat : memref<?xf32>,
    %Yflat : memref<?xf32>
  ) attributes {llvm.emit_c_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "arith.muli"(%n0, %n1) : (index, index) -> index
    %cout = "arith.muli"(%cout0, %cout1) : (index, index) -> index
    %oh = "arith.muli"(%oh0, %oh1) : (index, index) -> index
    %ow = "arith.muli"(%ow0, %ow1) : (index, index) -> index
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index

    %X = "memref.reinterpret_cast"(%Xflat, %c0, %n, %cin, %oh, %ow, %kh, %kw, %chw, %hw, %w, %c1, %w, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 6, 6>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %K = "memref.reinterpret_cast"(%Kflat, %c0, %cout, %cin, %kh, %kw, %cin_khkw, %khkw, %kw, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %Y = "memref.reinterpret_cast"(%Yflat, %c0, %n, %cout, %oh, %ow, %cout_ohow, %ohow, %ow, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    affine.for %ni = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        affine.for %ohi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          affine.for %owi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw) step 1 : index iter_args(%acc = %f0 : f32) {
              %ci = "arith.divui"(%p, %khkw) : (index, index) -> index
              %filter_p = "arith.remui"(%p, %khkw) : (index, index) -> index
              %khi = "arith.divui"(%filter_p, %kw) : (index, index) -> index
              %kwi = "arith.remui"(%filter_p, %kw) : (index, index) -> index
              %x = "memref.load"(%X, %ni, %ci, %ohi, %owi, %khi, %kwi) : (memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>, index, index, index, index, index, index) -> f32
              %k = "memref.load"(%K, %co, %ci, %khi, %kwi) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
              %mul = "arith.mulf"(%x, %k) : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) : (f32, f32) -> f32
              affine.yield %next : f32
            }
            "memref.store"(%sum, %Y, %ni, %co, %ohi, %owi) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
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
