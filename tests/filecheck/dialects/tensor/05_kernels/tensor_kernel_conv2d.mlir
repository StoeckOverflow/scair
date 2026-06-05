// Purpose: Conv2D kernel coverage: the structural experiment IR plus call-site verifier cases.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

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
    %X : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
    %K : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
    %Y : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %cin = "dtensor.shape.to_index"(%cin_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %kh = "dtensor.shape.to_index"(%kh_nat) : (!dtensor.nat) -> index
    %kw = "dtensor.shape.to_index"(%kw_nat) : (!dtensor.nat) -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index

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
    %n_nat : !dtensor.nat,
    %cin_nat : !dtensor.nat,
    %h_nat : !dtensor.nat,
    %w_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %kh_nat : !dtensor.nat,
    %kw_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %X : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
    %K : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
    %Y : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>
  ) {
    %kernel = func.constant @conv2d_dynamic : (!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.call_indirect"(%kernel, %n_nat, %cin_nat, %h_nat, %w_nat, %cout_nat, %kh_nat, %kw_nat, %oh_nat, %ow_nat, %X, %K, %Y)
      : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat,
          !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> (),
         !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.return"() : () -> ()
  }

  func.func @call_conv2d_dynamic_bad(
    %n_nat : !dtensor.nat,
    %cin_nat : !dtensor.nat,
    %h_nat : !dtensor.nat,
    %w_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %kh_nat : !dtensor.nat,
    %kw_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %X : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
    %K : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
    %Y_bad : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat], f32>
  ) {
    %kernel = func.constant @conv2d_dynamic : (!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    // expected-error @below {{func.call_indirect: argument types}}
    "func.call_indirect"(%kernel, %n_nat, %cin_nat, %h_nat, %w_nat, %cout_nat, %kh_nat, %kw_nat, %oh_nat, %ow_nat, %X, %K, %Y_bad)
      : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat,
          !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
          !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> (),
         !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32>,
         !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY: "func.func"() <{sym_name = "conv2d_dynamic", function_type = (!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%0, %1, %2, %3, %4, %5], f32>, !d_memref.memref<[%6, %1, %4, %5], f32>, !d_memref.memref<[%0, %6, %2, %3], f32>) -> ()}>
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%0, %1, %2, %3, %4, %5], f32>, index, index, index, index, index, index) -> f32
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%6, %1, %4, %5], f32>, index, index, index, index) -> f32
// VERIFY: "d_memref.store"({{.*}}) : (f32, !d_memref.memref<[%0, %6, %2, %3], f32>, index, index, index, index) -> ()
// VERIFY: "func.func"() <{sym_name = "call_conv2d_dynamic_good"
// VERIFY: "func.constant"() <{value = @conv2d_dynamic}> : () -> (!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%7, %8, %9, %10, %11, %12], f32>, !d_memref.memref<[%13, %8, %11, %12], f32>, !d_memref.memref<[%7, %13, %9, %10], f32>) -> ()
// VERIFY: "func.call_indirect"({{.*}}) : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%7, %8, %9, %10, %11, %12], f32>, !d_memref.memref<[%13, %8, %11, %12], f32>, !d_memref.memref<[%7, %13, %9, %10], f32>) -> (), !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%7, %8, %9, %10, %11, %12], f32>, !d_memref.memref<[%13, %8, %11, %12], f32>, !d_memref.memref<[%7, %13, %9, %10], f32>) -> ()
// VERIFY: "func.func"() <{sym_name = "call_conv2d_dynamic_bad"
// VERIFY: "func.constant"() <{value = @conv2d_dynamic}> : () -> (!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%14, %15, %16, %17, %18, %19], f32>, !d_memref.memref<[%20, %15, %18, %19], f32>, !d_memref.memref<[%14, %20, %16, %17], f32>) -> ()
// VERIFY: "func.call_indirect"({{.*}}) : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%14, %15, %16, %17, %18, %19], f32>, !d_memref.memref<[%20, %15, %18, %19], f32>, !d_memref.memref<[%14, %20, %16, %17], f32>) -> (), !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !dtensor.nat, !d_memref.memref<[%14, %15, %16, %17, %18, %19], f32>, !d_memref.memref<[%20, %15, %18, %19], f32>, !d_memref.memref<[%14, %15, %16, %17], f32>) -> ()
// DIAG: func.call_indirect: argument types
// DIAG-SAME: do not match callee input types
