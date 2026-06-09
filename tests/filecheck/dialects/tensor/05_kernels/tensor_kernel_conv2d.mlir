// Purpose: Conv2D kernel coverage: the structural experiment IR plus call-site verifier cases.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

builtin.module {
  func.func @conv2d_dynamic(
    %n_nat : !d_tensor.nat,
    %cin_nat : !d_tensor.nat,
    %h_nat : !d_tensor.nat,
    %w_nat : !d_tensor.nat,
    %cout_nat : !d_tensor.nat,
    %kh_nat : !d_tensor.nat,
    %kw_nat : !d_tensor.nat,
    %oh_nat : !d_tensor.nat,
    %ow_nat : !d_tensor.nat,
    %X : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
    %K : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
    %Y : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "d_tensor.shape.to_index"(%n_nat) : (!d_tensor.nat) -> index
    %cin = "d_tensor.shape.to_index"(%cin_nat) : (!d_tensor.nat) -> index
    %cout = "d_tensor.shape.to_index"(%cout_nat) : (!d_tensor.nat) -> index
    %kh = "d_tensor.shape.to_index"(%kh_nat) : (!d_tensor.nat) -> index
    %kw = "d_tensor.shape.to_index"(%kw_nat) : (!d_tensor.nat) -> index
    %oh = "d_tensor.shape.to_index"(%oh_nat) : (!d_tensor.nat) -> index
    %ow = "d_tensor.shape.to_index"(%ow_nat) : (!d_tensor.nat) -> index

    d_affine.for %n_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %co = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cout) step 1 : index {
        d_affine.for %oh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%oh) step 1 : index {
          d_affine.for %ow_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ow) step 1 : index {
            %sum_ci = d_affine.for %ci = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin) step 1 : index iter_args(%acc_ci = %f0 : f32) {
              %sum_kh = d_affine.for %kh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kh) step 1 : index iter_args(%acc_kh = %acc_ci : f32) {
                %sum_kw = d_affine.for %kw_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kw) step 1 : index iter_args(%acc_kw = %acc_kh : f32) {
                  %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32> -> f32
                  %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32> -> f32
                  %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  %next = "arith.addf"(%acc_kw, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  d_affine.yield %next : (f32)
                }
                d_affine.yield %sum_kw : (f32)
              }
              d_affine.yield %sum_kh : (f32)
            }
            d_memref.store %sum_ci, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>
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

  func.func @call_conv2d_dynamic_good(
    %n_nat : !d_tensor.nat,
    %cin_nat : !d_tensor.nat,
    %h_nat : !d_tensor.nat,
    %w_nat : !d_tensor.nat,
    %cout_nat : !d_tensor.nat,
    %kh_nat : !d_tensor.nat,
    %kw_nat : !d_tensor.nat,
    %oh_nat : !d_tensor.nat,
    %ow_nat : !d_tensor.nat,
    %X : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
    %K : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
    %Y : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>
  ) {
    %kernel = func.constant @conv2d_dynamic : (!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.call_indirect"(%kernel, %n_nat, %cin_nat, %h_nat, %w_nat, %cout_nat, %kh_nat, %kw_nat, %oh_nat, %ow_nat, %X, %K, %Y)
      : ((!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat,
          !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> (),
         !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.return"() : () -> ()
  }

  func.func @call_conv2d_dynamic_bad(
    %n_nat : !d_tensor.nat,
    %cin_nat : !d_tensor.nat,
    %h_nat : !d_tensor.nat,
    %w_nat : !d_tensor.nat,
    %cout_nat : !d_tensor.nat,
    %kh_nat : !d_tensor.nat,
    %kw_nat : !d_tensor.nat,
    %oh_nat : !d_tensor.nat,
    %ow_nat : !d_tensor.nat,
    %X : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
    %K : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
    %Y_bad : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat], f32>
  ) {
    %kernel = func.constant @conv2d_dynamic : (!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    // expected-error @below {{func.call_indirect: argument types}}
    "func.call_indirect"(%kernel, %n_nat, %cin_nat, %h_nat, %w_nat, %cout_nat, %kh_nat, %kw_nat, %oh_nat, %ow_nat, %X, %K, %Y_bad)
      : ((!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat,
          !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> (),
         !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY: "func.func"() <{sym_name = "conv2d_dynamic", function_type = (!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[N:[0-9]+]], %[[CIN:[0-9]+]], %[[OH:[0-9]+]], %[[OW:[0-9]+]], %[[KH:[0-9]+]], %[[KW:[0-9]+]]], f32>{{,}} !d_memref.memref<[%[[COUT:[0-9]+]], %[[CIN]], %[[KH]], %[[KW]]], f32>{{,}} !d_memref.memref<[%[[N]], %[[COUT]], %[[OH]], %[[OW]]], f32>) -> ()}>
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%[[N]], %[[CIN]], %[[OH]], %[[OW]], %[[KH]], %[[KW]]], f32>, index, index, index, index, index, index) -> f32
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%[[COUT]], %[[CIN]], %[[KH]], %[[KW]]], f32>, index, index, index, index) -> f32
// VERIFY: "d_memref.store"({{.*}}) : (f32, !d_memref.memref<[%[[N]], %[[COUT]], %[[OH]], %[[OW]]], f32>, index, index, index, index) -> ()
// VERIFY: "func.func"() <{sym_name = "call_conv2d_dynamic_good"
// VERIFY: %[[GOOD_KERNEL:[0-9]+]] = "func.constant"() <{value = @conv2d_dynamic}> : () -> (!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[GOOD_N:[0-9]+]], %[[GOOD_CIN:[0-9]+]], %[[GOOD_OH:[0-9]+]], %[[GOOD_OW:[0-9]+]], %[[GOOD_KH:[0-9]+]], %[[GOOD_KW:[0-9]+]]], f32>{{,}} !d_memref.memref<[%[[GOOD_COUT:[0-9]+]], %[[GOOD_CIN]], %[[GOOD_KH]], %[[GOOD_KW]]], f32>{{,}} !d_memref.memref<[%[[GOOD_N]], %[[GOOD_COUT]], %[[GOOD_OH]], %[[GOOD_OW]]], f32>) -> ()
// VERIFY: "func.call_indirect"(%[[GOOD_KERNEL]], %[[GOOD_N]], %[[GOOD_CIN]], %0, %1, %[[GOOD_COUT]], %[[GOOD_KH]], %[[GOOD_KW]], %[[GOOD_OH]], %[[GOOD_OW]], %2, %3, %4) : ((!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[GOOD_N]], %[[GOOD_CIN]], %[[GOOD_OH]], %[[GOOD_OW]], %[[GOOD_KH]], %[[GOOD_KW]]], f32>{{,}} !d_memref.memref<[%[[GOOD_COUT]], %[[GOOD_CIN]], %[[GOOD_KH]], %[[GOOD_KW]]], f32>{{,}} !d_memref.memref<[%[[GOOD_N]], %[[GOOD_COUT]], %[[GOOD_OH]], %[[GOOD_OW]]], f32>) -> (), !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[GOOD_N]], %[[GOOD_CIN]], %[[GOOD_OH]], %[[GOOD_OW]], %[[GOOD_KH]], %[[GOOD_KW]]], f32>{{,}} !d_memref.memref<[%[[GOOD_COUT]], %[[GOOD_CIN]], %[[GOOD_KH]], %[[GOOD_KW]]], f32>{{,}} !d_memref.memref<[%[[GOOD_N]], %[[GOOD_COUT]], %[[GOOD_OH]], %[[GOOD_OW]]], f32>) -> ()
// VERIFY: "func.func"() <{sym_name = "call_conv2d_dynamic_bad"
// VERIFY: %[[BAD_KERNEL:[0-9]+]] = "func.constant"() <{value = @conv2d_dynamic}> : () -> (!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[BAD_N:[0-9]+]], %[[BAD_CIN:[0-9]+]], %[[BAD_OH:[0-9]+]], %[[BAD_OW:[0-9]+]], %[[BAD_KH:[0-9]+]], %[[BAD_KW:[0-9]+]]], f32>{{,}} !d_memref.memref<[%[[BAD_COUT:[0-9]+]], %[[BAD_CIN]], %[[BAD_KH]], %[[BAD_KW]]], f32>{{,}} !d_memref.memref<[%[[BAD_N]], %[[BAD_COUT]], %[[BAD_OH]], %[[BAD_OW]]], f32>) -> ()
// VERIFY: "func.call_indirect"(%[[BAD_KERNEL]], %[[BAD_N]], %[[BAD_CIN]], %0, %1, %[[BAD_COUT]], %[[BAD_KH]], %[[BAD_KW]], %[[BAD_OH]], %[[BAD_OW]], %2, %3, %4) : ((!d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[BAD_N]], %[[BAD_CIN]], %[[BAD_OH]], %[[BAD_OW]], %[[BAD_KH]], %[[BAD_KW]]], f32>{{,}} !d_memref.memref<[%[[BAD_COUT]], %[[BAD_CIN]], %[[BAD_KH]], %[[BAD_KW]]], f32>{{,}} !d_memref.memref<[%[[BAD_N]], %[[BAD_COUT]], %[[BAD_OH]], %[[BAD_OW]]], f32>) -> (), !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_tensor.nat, !d_memref.memref<[%[[BAD_N]], %[[BAD_CIN]], %[[BAD_OH]], %[[BAD_OW]], %[[BAD_KH]], %[[BAD_KW]]], f32>{{,}} !d_memref.memref<[%[[BAD_COUT]], %[[BAD_CIN]], %[[BAD_KH]], %[[BAD_KW]]], f32>{{,}} !d_memref.memref<[%[[BAD_N]], %[[BAD_CIN]], %[[BAD_OH]], %[[BAD_OW]]], f32>) -> ()
// DIAG: func.call_indirect: argument types
// DIAG-SAME: do not match callee input types
