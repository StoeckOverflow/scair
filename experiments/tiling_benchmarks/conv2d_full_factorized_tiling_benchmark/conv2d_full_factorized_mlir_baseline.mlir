module {
  func.func @conv2d_full_factorized_tiling(
      %n0 : index, %n1 : index,
      %cin0 : index, %cin1 : index,
      %h : index, %w : index,
      %cout0 : index, %cout1 : index,
      %kh : index, %kw : index,
      %oh0 : index, %oh1 : index,
      %ow0 : index, %ow1 : index,
      %Xflat : memref<?xf32>,
      %Kflat : memref<?xf32>,
      %Yflat : memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %f0 = arith.constant 0.0 : f32
    %n = arith.muli %n0, %n1 : index
    %cin = arith.muli %cin0, %cin1 : index
    %cout = arith.muli %cout0, %cout1 : index
    %oh = arith.muli %oh0, %oh1 : index
    %ow = arith.muli %ow0, %ow1 : index
    %hw = arith.muli %h, %w : index
    %chw = arith.muli %cin, %hw : index
    %khkw = arith.muli %kh, %kw : index
    %red_tile = arith.muli %cin1, %khkw : index
    %cin_khkw = arith.muli %cin0, %red_tile : index
    %ohow = arith.muli %oh, %ow : index
    %cout_ohow = arith.muli %cout, %ohow : index

    %X = memref.reinterpret_cast %Xflat to
      offset: [%c0],
      sizes: [%n, %cin, %oh, %ow, %kh, %kw],
      strides: [%chw, %hw, %w, %c1, %w, %c1]
    : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %K = memref.reinterpret_cast %Kflat to
      offset: [%c0],
      sizes: [%cout, %cin, %kh, %kw],
      strides: [%cin_khkw, %khkw, %kw, %c1]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %Y = memref.reinterpret_cast %Yflat to
      offset: [%c0],
      sizes: [%n, %cout, %oh, %ow],
      strides: [%cout_ohow, %ohow, %ow, %c1]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    affine.for %ni = 0 to %n {
      affine.for %co = 0 to %cout {
        affine.for %ohi = 0 to %oh {
          affine.for %owi = 0 to %ow {
            %sum = affine.for %p = 0 to %cin_khkw iter_args(%acc = %f0) -> f32 {
              %ci = arith.divui %p, %khkw : index
              %filter_p = arith.remui %p, %khkw : index
              %khi = arith.divui %filter_p, %kw : index
              %kwi = arith.remui %filter_p, %kw : index
              %x = memref.load %X[%ni, %ci, %ohi, %owi, %khi, %kwi] : memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
              %k = memref.load %K[%co, %ci, %khi, %kwi] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
              %mul = arith.mulf %x, %k : f32
              %next = arith.addf %acc, %mul : f32
              affine.yield %next : f32
            }
            memref.store %sum, %Y[%ni, %co, %ohi, %owi] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
          }
        }
      }
    }
    return
  }
}
