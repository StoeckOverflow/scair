module {
  func.func @conv2d_dynamic(
      %n : index, %cin : index, %h : index, %w : index,
      %cout : index, %kh : index, %kw : index,
      %oh : index, %ow : index,
      %Xflat : memref<?xf32>,
      %Kflat : memref<?xf32>,
      %Yflat : memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %f0 = arith.constant 0.0 : f32
    %hw = arith.muli %h, %w : index
    %chw = arith.muli %cin, %hw : index
    %khkw = arith.muli %kh, %kw : index
    %cin_khkw = arith.muli %cin, %khkw : index
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

    affine.for %n_idx = 0 to %n {
      affine.for %co = 0 to %cout {
        affine.for %oh_idx = 0 to %oh {
          affine.for %ow_idx = 0 to %ow {
            %sum_ci = affine.for %ci = 0 to %cin iter_args(%acc_ci = %f0) -> f32 {
              %sum_kh = affine.for %kh_idx = 0 to %kh iter_args(%acc_kh = %acc_ci) -> f32 {
                %sum_kw = affine.for %kw_idx = 0 to %kw iter_args(%acc_kw = %acc_kh) -> f32 {
                  %x = memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
                  %k = memref.load %K[%co, %ci, %kh_idx, %kw_idx] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                  %mul = arith.mulf %x, %k : f32
                  %next = arith.addf %acc_kw, %mul : f32
                  affine.yield %next : f32
                }
                affine.yield %sum_kw : f32
              }
              affine.yield %sum_kh : f32
            }
            memref.store %sum_ci, %Y[%n_idx, %co, %oh_idx, %ow_idx] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
          }
        }
      }
    }

    return
  }
}
