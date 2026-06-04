// Purpose: Kernel-shaped conv2d coverage mirroring experiment IR with explicit output-shape assertions.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

// Positive: experiment-style dynamic conv2d kernel with matching X/K/Y shapes.
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
    %Xflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Yflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %cin = "dtensor.shape.to_index"(%cin_nat) : (!dtensor.nat) -> index
    %h = "dtensor.shape.to_index"(%h_nat) : (!dtensor.nat) -> index
    %w = "dtensor.shape.to_index"(%w_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %kh = "dtensor.shape.to_index"(%kh_nat) : (!dtensor.nat) -> index
    %kw = "dtensor.shape.to_index"(%kw_nat) : (!dtensor.nat) -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index
    %hw = "arith.muli"(%h, %w) : (index, index) -> index
    %chw = "arith.muli"(%cin, %hw) : (index, index) -> index
    %khkw = "arith.muli"(%kh, %kw) : (index, index) -> index
    %cin_khkw = "arith.muli"(%cin, %khkw) : (index, index) -> index
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
            %sum_ci = d_affine.for %ci = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%cin) step 1 : index iter_args(%acc_ci = %f0 : f32) {
              %sum_kh = d_affine.for %kh_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kh) step 1 : index iter_args(%acc_kh = %acc_ci : f32) {
                %sum_kw = d_affine.for %kw_idx = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%kw) step 1 : index iter_args(%acc_kw = %acc_kh : f32) {
                  %x = d_memref.load %X[%n_idx, %ci, %oh_idx, %ow_idx, %kh_idx, %kw_idx] : !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat, %kh_nat, %kw_nat], f32, offset: 0, strides: [%chw, %hw, %w, %c1, %w, %c1]> -> f32
                  %k = d_memref.load %K[%co, %ci, %kh_idx, %kw_idx] : !d_memref.memref<[%cout_nat, %cin_nat, %kh_nat, %kw_nat], f32, offset: 0, strides: [%cin_khkw, %khkw, %kw, %c1]> -> f32
                  %mul = "arith.mulf"(%x, %k) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  %next = "arith.addf"(%acc_kw, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                  d_affine.yield %next : (f32)
                }
                d_affine.yield %sum_kw : (f32)
              }
              d_affine.yield %sum_kh : (f32)
            }
            d_memref.store %sum_ci, %Y[%n_idx, %co, %oh_idx, %ow_idx] : f32, !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }

    %shape_ok = d_memref.cast %Y
      : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
     -> !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
    "test.keep_kernel_conv2d"(%shape_ok) : (!d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY-LABEL: func.func @conv2d_dynamic(
// VERIFY-SAME: [[BATCH:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[CIN:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[H:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[W:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[COUT:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[KH:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[KW:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[OH:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[OW:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[XFLAT:%[0-9]+]]: !d_memref.memref<[], f32>
// VERIFY-SAME: [[KFLAT:%[0-9]+]]: !d_memref.memref<[], f32>
// VERIFY-SAME: [[YFLAT:%[0-9]+]]: !d_memref.memref<[], f32>
// VERIFY: [[ONE:%[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// VERIFY: [[CIN_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[CIN]]) : (!dtensor.nat) -> index
// VERIFY: [[H_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[H]]) : (!dtensor.nat) -> index
// VERIFY: [[W_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[W]]) : (!dtensor.nat) -> index
// VERIFY: [[COUT_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[COUT]]) : (!dtensor.nat) -> index
// VERIFY: [[KH_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[KH]]) : (!dtensor.nat) -> index
// VERIFY: [[KW_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[KW]]) : (!dtensor.nat) -> index
// VERIFY: [[OH_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[OH]]) : (!dtensor.nat) -> index
// VERIFY: [[OW_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[OW]]) : (!dtensor.nat) -> index
// VERIFY: [[HW:%[0-9]+]] = "arith.muli"([[H_IDX]], [[W_IDX]])
// VERIFY: [[CHW:%[0-9]+]] = "arith.muli"([[CIN_IDX]], [[HW]])
// VERIFY: [[KHKW:%[0-9]+]] = "arith.muli"([[KH_IDX]], [[KW_IDX]])
// VERIFY: [[CIN_KHKW:%[0-9]+]] = "arith.muli"([[CIN_IDX]], [[KHKW]])
// VERIFY: [[OHOW:%[0-9]+]] = "arith.muli"([[OH_IDX]], [[OW_IDX]])
// VERIFY: [[COUT_OHOW:%[0-9]+]] = "arith.muli"([[COUT_IDX]], [[OHOW]])
// VERIFY: [[X:%[0-9]+]] = d_memref.reinterpret_cast [[XFLAT]]
// VERIFY-NEXT: : !d_memref.memref<[], f32> to !d_memref.memref<{{\[}}[[BATCH]], [[CIN]], [[OH]], [[OW]], [[KH]], [[KW]]], f32, offset: 0, strides: {{\[}}[[CHW]], [[HW]], [[W_IDX]], [[ONE]], [[W_IDX]], [[ONE]]]>
// VERIFY: [[KERNEL:%[0-9]+]] = d_memref.reinterpret_cast [[KFLAT]]
// VERIFY-NEXT: : !d_memref.memref<[], f32> to !d_memref.memref<{{\[}}[[COUT]], [[CIN]], [[KH]], [[KW]]], f32, offset: 0, strides: {{\[}}[[CIN_KHKW]], [[KHKW]], [[KW_IDX]], [[ONE]]]>
// VERIFY: [[Y:%[0-9]+]] = d_memref.reinterpret_cast [[YFLAT]]
// VERIFY-NEXT: : !d_memref.memref<[], f32> to !d_memref.memref<{{\[}}[[BATCH]], [[COUT]], [[OH]], [[OW]]], f32, offset: 0, strides: {{\[}}[[COUT_OHOW]], [[OHOW]], [[OW_IDX]], [[ONE]]]>
// VERIFY: [[XVAL:%[0-9]+]] = d_memref.load [[X]][{{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}] : !d_memref.memref<{{\[}}[[BATCH]], [[CIN]], [[OH]], [[OW]], [[KH]], [[KW]]], f32, offset: 0, strides: {{\[}}[[CHW]], [[HW]], [[W_IDX]], [[ONE]], [[W_IDX]], [[ONE]]]> -> f32
// VERIFY: [[KVAL:%[0-9]+]] = d_memref.load [[KERNEL]][{{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}] : !d_memref.memref<{{\[}}[[COUT]], [[CIN]], [[KH]], [[KW]]], f32, offset: 0, strides: {{\[}}[[CIN_KHKW]], [[KHKW]], [[KW_IDX]], [[ONE]]]> -> f32
// VERIFY: [[PROD:%[0-9]+]] = "arith.mulf"([[XVAL]], [[KVAL]])
// VERIFY: d_memref.store {{%[0-9]+}}, [[Y]][{{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}, {{%[0-9]+}}] : f32, !d_memref.memref<{{\[}}[[BATCH]], [[COUT]], [[OH]], [[OW]]], f32, offset: 0, strides: {{\[}}[[COUT_OHOW]], [[OHOW]], [[OW_IDX]], [[ONE]]]>
// VERIFY: [[SHAPE_OK:%[0-9]+]] = d_memref.cast [[Y]] : !d_memref.memref<{{\[}}[[BATCH]], [[COUT]], [[OH]], [[OW]]], f32, offset: 0, strides: {{\[}}[[COUT_OHOW]], [[OHOW]], [[OW_IDX]], [[ONE]]]> -> !d_memref.memref<{{\[}}[[BATCH]], [[COUT]], [[OH]], [[OW]]], f32, offset: 0, strides: {{\[}}[[COUT_OHOW]], [[OHOW]], [[OW_IDX]], [[ONE]]]>
// VERIFY: "test.keep_kernel_conv2d"([[SHAPE_OK]]) : (!d_memref.memref<{{\[}}[[BATCH]], [[COUT]], [[OH]], [[OW]]], f32, offset: 0, strides: {{\[}}[[COUT_OHOW]], [[OHOW]], [[OW_IDX]], [[ONE]]]>) -> ()

// -----

// Negative: same experiment-style conv2d output view, but Y is asserted as N x Cin x OH x OW instead of N x Cout x OH x OW.
builtin.module {
  func.func @conv2d_dynamic_bad_output(
    %n_nat : !dtensor.nat,
    %cin_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %Yflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index
    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat],
             f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
    // expected-error @below {{d_memref.cast: expected pairwise SSA-identical dims}}
    %bad = d_memref.cast %Y
      : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
     -> !d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
    "test.keep_kernel_conv2d_bad"(%bad) : (!d_memref.memref<[%n_nat, %cin_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>) -> ()
    "func.return"() : () -> ()
  }
}

// DIAG: "d_memref.cast"({{%[0-9]+}}) : (!d_memref.memref<{{\[}}%{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}], f32, offset: 0, strides: {{\[}}%{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}]>) -> !d_memref.memref<{{\[}}%{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}], f32, offset: 0, strides: {{\[}}%{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}]>
// DIAG: d_memref.cast: expected pairwise SSA-identical dims

// -----

// Negative: subview result dims must be proven by the size operands, not arbitrary index values.
builtin.module {
  func.func @conv2d_subview_size_without_shape_provenance(
    %n_nat : !dtensor.nat,
    %cout_nat : !dtensor.nat,
    %oh_nat : !dtensor.nat,
    %ow_nat : !dtensor.nat,
    %Yflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %cout = "dtensor.shape.to_index"(%cout_nat) : (!dtensor.nat) -> index
    %oh = "dtensor.shape.to_index"(%oh_nat) : (!dtensor.nat) -> index
    %ow = "dtensor.shape.to_index"(%ow_nat) : (!dtensor.nat) -> index
    %ohow = "arith.muli"(%oh, %ow) : (index, index) -> index
    %cout_ohow = "arith.muli"(%cout, %ohow) : (index, index) -> index
    %Y = d_memref.reinterpret_cast %Yflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat],
             f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
    %bad_size = "arith.addi"(%n, %c0) : (index, index) -> index
    // expected-error @below {{d_memref.subview: size provenance mismatch at axis 0}}
    %bad = d_memref.subview %Y[%c0, %c0, %c0, %c0][%bad_size, %cout, %oh, %ow][%c1, %c1, %c1, %c1]
      : !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32, offset: 0, strides: [%cout_ohow, %ohow, %ow, %c1]>
     -> !d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>
    "test.keep_kernel_subview_bad"(%bad) : (!d_memref.memref<[%n_nat, %cout_nat, %oh_nat, %ow_nat], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// DIAG: d_memref.subview: size provenance mismatch at axis 0; expected result dim to match size operand via dtensor.shape.to_index
