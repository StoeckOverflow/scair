// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-size-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=EXACT --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-size-products,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=TAIL
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-size-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=SIMPLIFIED --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @conv2d_tiling_dynamic(
    %n_size : !d_tensor.size,
    %cin_size : !d_tensor.size,
    %h_size : !d_tensor.size,
    %w_size : !d_tensor.size,
    %cout_size : !d_tensor.size,
    %kh_size : !d_tensor.pos_size,
    %kw_size : !d_tensor.pos_size,
    %oh_size : !d_tensor.size,
    %ow_size : !d_tensor.size,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) {
    %khkw_size = "d_tensor.size.mul"(%kh_size, %kw_size) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
    %cin_khkw_size = "d_tensor.size.mul"(%cin_size, %khkw_size) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %hw = "d_tensor.size.mul"(%h_size, %w_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %chw = "d_tensor.size.mul"(%cin_size, %hw) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %ohow = "d_tensor.size.mul"(%oh_size, %ow_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %cout_ohow = "d_tensor.size.mul"(%cout_size, %ohow) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size

    %X = d_memref.reinterpret_cast %Xflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size],
             f32, offset: 0, strides: [%chw, %hw, %w_size, %c1, %w_size, %c1]>

    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size],
             f32, offset: 0, strides: [%cin_khkw_size, %khkw_size, %kw_size, %c1]>

    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size],
             f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout_size) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh_size) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow_size) step 1 : index {
            %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin_khkw_size) step 1 : index iter_args(%acc = %f0 : f32) {
              %x = d_memref.load %X[%n_idx, %c0, %oh_idx, %ow_idx, %c0, %c0] : !d_memref.memref<[%n_size, %cin_size, %oh_size, %ow_size, %kh_size, %kw_size], f32, offset: 0, strides: [%chw, %hw, %w_size, %c1, %w_size, %c1]> -> f32
              %k = d_memref.load %K[%co, %c0, %c0, %c0] : !d_memref.memref<[%cout_size, %cin_size, %kh_size, %kw_size], f32, offset: 0, strides: [%cin_khkw_size, %khkw_size, %kw_size, %c1]> -> f32
              %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n_size, %cout_size, %oh_size, %ow_size], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow_size, %c1]>
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

// EXACT: #map = affine_map<(d0)[] -> (d0)>
// EXACT-LABEL: func.func @conv2d_tiling_dynamic
// EXACT-SAME: %[[N_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[CIN_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[H_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[W_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[COUT_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[KH_NAT:[0-9]+]]: !d_tensor.pos_size
// EXACT-SAME: %[[KW_NAT:[0-9]+]]: !d_tensor.pos_size
// EXACT-SAME: %[[OH_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[OW_NAT:[0-9]+]]: !d_tensor.size
// EXACT-SAME: %[[XFLAT:[0-9]+]]: !d_memref.memref<[], f32>
// EXACT-SAME: %[[KFLAT:[0-9]+]]: !d_memref.memref<[], f32>
// EXACT-SAME: %[[YFLAT:[0-9]+]]: !d_memref.memref<[], f32>
// EXACT: %[[KHKW_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[KH_NAT]], %[[KW_NAT]]) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
// EXACT: %[[FULL_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[CIN_NAT]], %[[KHKW_NAT]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// EXACT: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EXACT: %[[C1:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// EXACT: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// EXACT: %[[HW:[0-9]+]] = "d_tensor.size.mul"(%[[H_NAT]], %[[W_NAT]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EXACT: %[[CHW:[0-9]+]] = "d_tensor.size.mul"(%[[CIN_NAT]], %[[HW]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EXACT: %[[OHOW:[0-9]+]] = "d_tensor.size.mul"(%[[OH_NAT]], %[[OW_NAT]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EXACT: %[[COUT_OHOW:[0-9]+]] = "d_tensor.size.mul"(%[[COUT_NAT]], %[[OHOW]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EXACT: %[[X:[0-9]+]] = d_memref.reinterpret_cast %[[XFLAT]]
// EXACT: %[[KERNEL:[0-9]+]] = d_memref.reinterpret_cast %[[KFLAT]]
// EXACT: %[[Y:[0-9]+]] = d_memref.reinterpret_cast %[[YFLAT]]
// EXACT: d_affine.for %[[NIDX:[0-9]+]] = #map(%[[C0]]) to #map(%[[N_NAT]]) step 1 : index {
// EXACT: d_affine.for %[[CO:[0-9]+]] = #map(%[[C0]]) to #map(%[[COUT_NAT]]) step 1 : index {
// EXACT: d_affine.for %[[OHIDX:[0-9]+]] = #map(%[[C0]]) to #map(%[[OH_NAT]]) step 1 : index {
// EXACT: d_affine.for %[[OWIDX:[0-9]+]] = #map(%[[C0]]) to #map(%[[OW_NAT]]) step 1 : index {
// EXACT: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0]]) to #map(%[[FULL_NAT]]) step %[[KW_NAT]] : !d_tensor.pos_size iter_args(%[[ACC:[0-9]+]] = %[[F0]] : f32) {
// EXACT: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[KW_NAT]]] : (index)[!d_tensor.pos_size] -> index
// EXACT: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : i32 iter_args(%[[INNER_ACC:[0-9]+]] = %[[ACC]] : f32) {
// EXACT: %[[XVAL:[0-9]+]] = d_memref.load %[[X]][%[[NIDX]], %[[C0]], %[[OHIDX]], %[[OWIDX]], %[[C0]], %[[C0]]]
// EXACT: %[[KVAL:[0-9]+]] = d_memref.load %[[KERNEL]][%[[CO]], %[[C0]], %[[C0]], %[[C0]]]
// EXACT: %[[MUL:[0-9]+]] = "arith.mulf"(%[[XVAL]], %[[KVAL]]) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
// EXACT: %[[NEXT:[0-9]+]] = "arith.addf"(%[[INNER_ACC]], %[[MUL]]) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
// EXACT: d_affine.yield %[[NEXT]] : (f32)
// EXACT: d_memref.store %[[SUM]], %[[Y]][%[[NIDX]], %[[CO]], %[[OHIDX]], %[[OWIDX]]]

// TAIL: #map = affine_map<(d0)[] -> (d0)>
// TAIL-LABEL: func.func @conv2d_tiling_dynamic
// TAIL-SAME: %[[N_NAT:[0-9]+]]: !d_tensor.size
// TAIL-SAME: %[[CIN_NAT:[0-9]+]]: !d_tensor.size
// TAIL-SAME: %[[H_NAT:[0-9]+]]: !d_tensor.size
// TAIL-SAME: %[[W_NAT:[0-9]+]]: !d_tensor.size
// TAIL-SAME: %[[COUT_NAT:[0-9]+]]: !d_tensor.size
// TAIL-SAME: %[[KH_NAT:[0-9]+]]: !d_tensor.pos_size
// TAIL-SAME: %[[KW_NAT:[0-9]+]]: !d_tensor.pos_size
// TAIL-SAME: %[[OH_NAT:[0-9]+]]: !d_tensor.size
// TAIL-SAME: %[[OW_NAT:[0-9]+]]: !d_tensor.size
// TAIL: %[[KHKW_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[KH_NAT]], %[[KW_NAT]]) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
// TAIL: %[[FULL_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[CIN_NAT]], %[[KHKW_NAT]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// TAIL: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// TAIL: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// TAIL: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0]]) to #map(%[[FULL_NAT]]) step %[[KW_NAT]] : !d_tensor.pos_size iter_args(%[[ACC:[0-9]+]] = %[[F0]] : f32) {
// TAIL: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[KW_NAT]]] : (index)[!d_tensor.pos_size] -> index
// TAIL: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%[[FULL_NAT]]] : (index)[!d_tensor.size] -> index
// TAIL: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32
// TAIL: d_memref.load

// SIMPLIFIED: #map = affine_map<(d0)[] -> (d0)>
// SIMPLIFIED-LABEL: func.func @conv2d_tiling_dynamic
// SIMPLIFIED-SAME: %[[N_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED-SAME: %[[CIN_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED-SAME: %[[H_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED-SAME: %[[W_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED-SAME: %[[COUT_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED-SAME: %[[KH_NAT:[0-9]+]]: !d_tensor.pos_size
// SIMPLIFIED-SAME: %[[KW_NAT:[0-9]+]]: !d_tensor.pos_size
// SIMPLIFIED-SAME: %[[OH_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED-SAME: %[[OW_NAT:[0-9]+]]: !d_tensor.size
// SIMPLIFIED: %[[KHKW_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[KH_NAT]], %[[KW_NAT]]) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
// SIMPLIFIED: %[[FULL_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[CIN_NAT]], %[[KHKW_NAT]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// SIMPLIFIED: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// SIMPLIFIED: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// SIMPLIFIED: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0]]) to #map(%[[FULL_NAT]]) step %[[KW_NAT]] : !d_tensor.pos_size iter_args(%[[ACC:[0-9]+]] = %[[F0]] : f32) {
// SIMPLIFIED: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[KW_NAT]]] : (index)[!d_tensor.pos_size] -> index
// SIMPLIFIED: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%[[FULL_NAT]]] : (index)[!d_tensor.size] -> index
// SIMPLIFIED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32
// SIMPLIFIED: d_memref.load
