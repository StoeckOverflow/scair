// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-nat-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=EXACT --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-nat-products,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=TAIL
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-nat-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=SIMPLIFIED --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @conv2d_tiling_dynamic(
    %n_nat : !d_tensor.nat,
    %cin_nat : !d_tensor.nat,
    %h_nat : !d_tensor.nat,
    %w_nat : !d_tensor.nat,
    %cout_nat : !d_tensor.nat,
    %kh_nat : !d_tensor.posnat,
    %kw_nat : !d_tensor.posnat,
    %oh_nat : !d_tensor.nat,
    %ow_nat : !d_tensor.nat,
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) {
    %khkw_nat = "d_tensor.nat.mul"(%kh_nat, %kw_nat) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
    %cin_khkw_nat = "d_tensor.nat.mul"(%cin_nat, %khkw_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat

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
        to !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat],
             f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]>

    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat],
             f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]>

    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat],
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
              %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]> -> f32
              %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]> -> f32
              %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
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
// EXACT-SAME: %[[N_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[CIN_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[H_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[W_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[COUT_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[KH_NAT:[0-9]+]]: !d_tensor.posnat
// EXACT-SAME: %[[KW_NAT:[0-9]+]]: !d_tensor.posnat
// EXACT-SAME: %[[OH_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[OW_NAT:[0-9]+]]: !d_tensor.nat
// EXACT-SAME: %[[XFLAT:[0-9]+]]: !d_memref.memref<[], f32>
// EXACT-SAME: %[[KFLAT:[0-9]+]]: !d_memref.memref<[], f32>
// EXACT-SAME: %[[YFLAT:[0-9]+]]: !d_memref.memref<[], f32>
// EXACT: %[[KHKW_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[KH_NAT]], %[[KW_NAT]]) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
// EXACT: %[[FULL_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[CIN_NAT]], %[[KHKW_NAT]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
// EXACT: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EXACT: %[[C1:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// EXACT: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// EXACT: %[[N:[0-9]+]] = "d_tensor.shape.to_index"(%[[N_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[CIN:[0-9]+]] = "d_tensor.shape.to_index"(%[[CIN_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[H:[0-9]+]] = "d_tensor.shape.to_index"(%[[H_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[W:[0-9]+]] = "d_tensor.shape.to_index"(%[[W_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[COUT:[0-9]+]] = "d_tensor.shape.to_index"(%[[COUT_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[KW:[0-9]+]] = "d_tensor.shape.to_index"(%[[KW_NAT]]) : (!d_tensor.posnat) -> index
// EXACT: %[[OH:[0-9]+]] = "d_tensor.shape.to_index"(%[[OH_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[OW:[0-9]+]] = "d_tensor.shape.to_index"(%[[OW_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[KHKW:[0-9]+]] = "d_tensor.shape.to_index"(%[[KHKW_NAT]]) : (!d_tensor.posnat) -> index
// EXACT: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[FULL_NAT]]) : (!d_tensor.nat) -> index
// EXACT: %[[HW:[0-9]+]] = "arith.muli"(%[[H]], %[[W]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// EXACT: %[[CHW:[0-9]+]] = "arith.muli"(%[[CIN]], %[[HW]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// EXACT: %[[OHOW:[0-9]+]] = "arith.muli"(%[[OH]], %[[OW]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// EXACT: %[[COUT_OHOW:[0-9]+]] = "arith.muli"(%[[COUT]], %[[OHOW]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// EXACT: %[[X:[0-9]+]] = d_memref.reinterpret_cast %[[XFLAT]]
// EXACT: %[[KERNEL:[0-9]+]] = d_memref.reinterpret_cast %[[KFLAT]]
// EXACT: %[[Y:[0-9]+]] = d_memref.reinterpret_cast %[[YFLAT]]
// EXACT: d_affine.for %[[NIDX:[0-9]+]] = #map(%[[C0]]) to #map(%[[N]]) step 1 : index {
// EXACT: d_affine.for %[[CO:[0-9]+]] = #map(%[[C0]]) to #map(%[[COUT]]) step 1 : index {
// EXACT: d_affine.for %[[OHIDX:[0-9]+]] = #map(%[[C0]]) to #map(%[[OH]]) step 1 : index {
// EXACT: d_affine.for %[[OWIDX:[0-9]+]] = #map(%[[C0]]) to #map(%[[OW]]) step 1 : index {
// EXACT: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0]]) to #map(%[[FULL]]) step %[[KW]] : index iter_args(%[[ACC:[0-9]+]] = %[[F0]] : f32) {
// EXACT: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[KW]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// EXACT: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : i32 iter_args(%[[INNER_ACC:[0-9]+]] = %[[ACC]] : f32) {
// EXACT: %[[CI:[0-9]+]] = "arith.divui"(%[[P]], %[[KHKW]]) : (index, index) -> index
// EXACT: %[[FILTER_P:[0-9]+]] = "arith.remui"(%[[P]], %[[KHKW]]) : (index, index) -> index
// EXACT: %[[KHIDX:[0-9]+]] = "arith.divui"(%[[FILTER_P]], %[[KW]]) : (index, index) -> index
// EXACT: %[[KWIDX:[0-9]+]] = "arith.remui"(%[[FILTER_P]], %[[KW]]) : (index, index) -> index
// EXACT: %[[XVAL:[0-9]+]] = d_memref.load %[[X]][%[[NIDX]], %[[CI]], %[[OHIDX]], %[[OWIDX]], %[[KHIDX]], %[[KWIDX]]]
// EXACT: %[[KVAL:[0-9]+]] = d_memref.load %[[KERNEL]][%[[CO]], %[[CI]], %[[KHIDX]], %[[KWIDX]]]
// EXACT: %[[MUL:[0-9]+]] = "arith.mulf"(%[[XVAL]], %[[KVAL]]) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
// EXACT: %[[NEXT:[0-9]+]] = "arith.addf"(%[[INNER_ACC]], %[[MUL]]) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
// EXACT: d_affine.yield %[[NEXT]] : (f32)
// EXACT: d_memref.store %[[SUM]], %[[Y]][%[[NIDX]], %[[CO]], %[[OHIDX]], %[[OWIDX]]]

// TAIL: #map = affine_map<(d0)[] -> (d0)>
// TAIL-LABEL: func.func @conv2d_tiling_dynamic
// TAIL-SAME: %[[N_NAT:[0-9]+]]: !d_tensor.nat
// TAIL-SAME: %[[CIN_NAT:[0-9]+]]: !d_tensor.nat
// TAIL-SAME: %[[H_NAT:[0-9]+]]: !d_tensor.nat
// TAIL-SAME: %[[W_NAT:[0-9]+]]: !d_tensor.nat
// TAIL-SAME: %[[COUT_NAT:[0-9]+]]: !d_tensor.nat
// TAIL-SAME: %[[KH_NAT:[0-9]+]]: !d_tensor.posnat
// TAIL-SAME: %[[KW_NAT:[0-9]+]]: !d_tensor.posnat
// TAIL-SAME: %[[OH_NAT:[0-9]+]]: !d_tensor.nat
// TAIL-SAME: %[[OW_NAT:[0-9]+]]: !d_tensor.nat
// TAIL: %[[KHKW_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[KH_NAT]], %[[KW_NAT]]) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
// TAIL: %[[FULL_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[CIN_NAT]], %[[KHKW_NAT]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
// TAIL: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// TAIL: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// TAIL: %[[KW:[0-9]+]] = "d_tensor.shape.to_index"(%[[KW_NAT]]) : (!d_tensor.posnat) -> index
// TAIL: %[[KHKW:[0-9]+]] = "d_tensor.shape.to_index"(%[[KHKW_NAT]]) : (!d_tensor.posnat) -> index
// TAIL: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[FULL_NAT]]) : (!d_tensor.nat) -> index
// TAIL: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0]]) to #map(%[[FULL]]) step %[[KW]] : index iter_args(%[[ACC:[0-9]+]] = %[[F0]] : f32) {
// TAIL: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[KW]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// TAIL: %[[CLAMPED:[0-9]+]] = "arith.minsi"(%[[TILE_END]], %[[FULL]]) : (index, index) -> index
// TAIL: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32
// TAIL: "arith.divui"(%[[P]], %[[KHKW]]) : (index, index) -> index

// SIMPLIFIED: #map = affine_map<(d0)[] -> (d0)>
// SIMPLIFIED-LABEL: func.func @conv2d_tiling_dynamic
// SIMPLIFIED-SAME: %[[N_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED-SAME: %[[CIN_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED-SAME: %[[H_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED-SAME: %[[W_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED-SAME: %[[COUT_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED-SAME: %[[KH_NAT:[0-9]+]]: !d_tensor.posnat
// SIMPLIFIED-SAME: %[[KW_NAT:[0-9]+]]: !d_tensor.posnat
// SIMPLIFIED-SAME: %[[OH_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED-SAME: %[[OW_NAT:[0-9]+]]: !d_tensor.nat
// SIMPLIFIED: %[[KHKW_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[KH_NAT]], %[[KW_NAT]]) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
// SIMPLIFIED: %[[FULL_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[CIN_NAT]], %[[KHKW_NAT]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
// SIMPLIFIED: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// SIMPLIFIED: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// SIMPLIFIED: %[[KW:[0-9]+]] = "d_tensor.shape.to_index"(%[[KW_NAT]]) : (!d_tensor.posnat) -> index
// SIMPLIFIED: %[[KHKW:[0-9]+]] = "d_tensor.shape.to_index"(%[[KHKW_NAT]]) : (!d_tensor.posnat) -> index
// SIMPLIFIED: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[FULL_NAT]]) : (!d_tensor.nat) -> index
// SIMPLIFIED: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0]]) to #map(%[[FULL]]) step %[[KW]] : index iter_args(%[[ACC:[0-9]+]] = %[[F0]] : f32) {
// SIMPLIFIED: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[KW]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// SIMPLIFIED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : i32
// SIMPLIFIED: "arith.divui"(%[[P]], %[[KHKW]]) : (index, index) -> index
