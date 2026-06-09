// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=EXACT --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=TAIL
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=SIMPLIFIED --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @conv2d_tiling_dynamic(
    %n : index,
    %cin : index,
    %h : index,
    %w : index,
    %cout : index,
    %oh : index,
    %ow : index,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %kh = "arith.constant"() <{value = 3 : index}> : () -> index
    %kw = "arith.constant"() <{value = 4 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
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

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw) step 1 : index iter_args(%acc = %f0 : f32) {
              %ci = "arith.divui"(%p, %khkw) : (index, index) -> index
              %filter_p = "arith.remui"(%p, %khkw) : (index, index) -> index
              %kh_idx = "arith.divui"(%filter_p, %kw) : (index, index) -> index
              %kw_idx = "arith.remui"(%filter_p, %kw) : (index, index) -> index
              %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n, %cin, %oh, %ow, %kh, %kw], f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]> -> f32
              %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout, %cin, %kh, %kw], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]> -> f32
              %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n, %cout, %oh, %ow], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
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

// EXACT-LABEL: func.func @conv2d_tiling_dynamic
// EXACT: %[[KH:[0-9]+]] = "arith.constant"() <{value = 3 : index}> : () -> index
// EXACT: %[[KW:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// EXACT: %[[KHKW:[0-9]+]] = "arith.constant"() <{value = 12 : index}> : () -> index
// EXACT: %[[FULL:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %[[KHKW]]) {{.*}} : (index, index) -> index
// EXACT: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%[[FULL]]) step 4 : i32 iter_args
// EXACT: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map{{[0-9]+}}(%[[TILE]]) step 1 : i32 iter_args
// EXACT: "arith.divui"(%[[P]], %[[KHKW]]) : (index, index) -> index
// EXACT: d_memref.store %[[SUM]]

// TAIL-LABEL: func.func @conv2d_tiling_dynamic
// TAIL: %[[KW:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// TAIL: %[[KHKW:[0-9]+]] = "arith.constant"() <{value = 12 : index}> : () -> index
// TAIL: %[[FULL:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %[[KHKW]]) {{.*}} : (index, index) -> index
// TAIL: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%[[FULL]]) step 4 : i32 iter_args
// TAIL: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[KW]])
// TAIL: %[[CLAMPED:[0-9]+]] = "arith.minsi"(%[[TILE_END]], %[[FULL]]) : (index, index) -> index
// TAIL: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32
// TAIL: "arith.divui"(%[[P]], %[[KHKW]]) : (index, index) -> index

// SIMPLIFIED-LABEL: func.func @conv2d_tiling_dynamic
// SIMPLIFIED: %[[KW:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// SIMPLIFIED: %[[KHKW:[0-9]+]] = "arith.constant"() <{value = 12 : index}> : () -> index
// SIMPLIFIED: %[[FULL:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %[[KHKW]]) {{.*}} : (index, index) -> index
// SIMPLIFIED: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%[[FULL]]) step 4 : i32 iter_args
// SIMPLIFIED: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[KW]])
// SIMPLIFIED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : i32
// SIMPLIFIED: "arith.divui"(%[[P]], %[[KHKW]]) : (index, index) -> index
