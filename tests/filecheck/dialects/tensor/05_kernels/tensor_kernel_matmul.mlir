// Purpose: Matmul kernel coverage: the strided experiment IR plus direct dtensor.matmul verifier cases.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

builtin.module {
  func.func @matmul_strided(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %k_nat : !dtensor.nat,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %A : !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
    %B : !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
    %C : !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }

  func.func @call_matmul_strided_good(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %k_nat : !dtensor.nat,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %A : !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
    %B : !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
    %C : !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
  ) {
    %kernel = func.constant @matmul_strided : (!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index,
         !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    "func.call_indirect"(%kernel, %n_nat, %m_nat, %k_nat, %a_stride0, %a_stride1, %b_stride0, %b_stride1, %c_stride0, %c_stride1, %A, %B, %C)
      : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index,
          !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
          !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
          !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> (),
         !dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index,
         !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    "func.return"() : () -> ()
  }

  func.func @call_matmul_strided_bad(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %k_nat : !dtensor.nat,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %A : !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
    %B : !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
    %C_bad : !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
  ) {
    %kernel = func.constant @matmul_strided : (!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index,
         !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    // expected-error @below {{func.call_indirect: argument types}}
    "func.call_indirect"(%kernel, %n_nat, %m_nat, %k_nat, %a_stride0, %a_stride1, %b_stride0, %b_stride1, %c_stride0, %c_stride1, %A, %B, %C_bad)
      : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index,
          !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
          !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
          !d_memref.memref<[%n_nat, %m_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> (),
         !dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index,
         !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_nat, %m_nat], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_nat, %k_nat], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY: "func.func"() <{sym_name = "matmul_strided", function_type = (!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%2, %3]>, !d_memref.memref<[%1, %4], f32, offset: 0, strides: [%5, %6]>, !d_memref.memref<[%0, %4], f32, offset: 0, strides: [%7, %8]>) -> ()}>
// VERIFY: "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// VERIFY: "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
// VERIFY: "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%0, %1], f32, offset: 0, strides: [%2, %3]>, index, index) -> f32
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%1, %4], f32, offset: 0, strides: [%5, %6]>, index, index) -> f32
// VERIFY: "d_memref.store"({{.*}}) : (f32, !d_memref.memref<[%0, %4], f32, offset: 0, strides: [%7, %8]>, index, index) -> ()
// VERIFY: "func.func"() <{sym_name = "call_matmul_strided_good"
// VERIFY: "func.constant"() <{value = @matmul_strided}> : () -> (!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%9, %10], f32, offset: 0, strides: [%11, %12]>, !d_memref.memref<[%10, %13], f32, offset: 0, strides: [%14, %15]>, !d_memref.memref<[%9, %13], f32, offset: 0, strides: [%16, %17]>) -> ()
// VERIFY: "func.call_indirect"({{.*}}) : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%9, %10], f32, offset: 0, strides: [%11, %12]>, !d_memref.memref<[%10, %13], f32, offset: 0, strides: [%14, %15]>, !d_memref.memref<[%9, %13], f32, offset: 0, strides: [%16, %17]>) -> (), !dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%9, %10], f32, offset: 0, strides: [%11, %12]>, !d_memref.memref<[%10, %13], f32, offset: 0, strides: [%14, %15]>, !d_memref.memref<[%9, %13], f32, offset: 0, strides: [%16, %17]>) -> ()
// VERIFY: "func.func"() <{sym_name = "call_matmul_strided_bad"
// VERIFY: "func.constant"() <{value = @matmul_strided}> : () -> (!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%18, %19], f32, offset: 0, strides: [%20, %21]>, !d_memref.memref<[%19, %22], f32, offset: 0, strides: [%23, %24]>, !d_memref.memref<[%18, %22], f32, offset: 0, strides: [%25, %26]>) -> ()
// VERIFY: "func.call_indirect"({{.*}}) : ((!dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%18, %19], f32, offset: 0, strides: [%20, %21]>, !d_memref.memref<[%19, %22], f32, offset: 0, strides: [%23, %24]>, !d_memref.memref<[%18, %22], f32, offset: 0, strides: [%25, %26]>) -> (), !dtensor.nat, !dtensor.nat, !dtensor.nat, index, index, index, index, index, index, !d_memref.memref<[%18, %19], f32, offset: 0, strides: [%20, %21]>, !d_memref.memref<[%19, %22], f32, offset: 0, strides: [%23, %24]>, !d_memref.memref<[%18, %19], f32, offset: 0, strides: [%25, %26]>) -> ()
// DIAG: func.call_indirect: argument types
// DIAG-SAME: do not match callee input types

// -----

// Positive: direct tensor-dialect matmul function.
builtin.module {
  func.func @tensor_matmul_function_ok(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %k_nat : !dtensor.nat,
    %A : !dtensor.tensor<[%n_nat, %k_nat], f32>,
    %B : !dtensor.tensor<[%k_nat, %m_nat], f32>
  ) {
    %C = "dtensor.matmul"(%A, %B)
      : (!dtensor.tensor<[%n_nat, %k_nat], f32>, !dtensor.tensor<[%k_nat, %m_nat], f32>)
     -> !dtensor.tensor<[%n_nat, %m_nat], f32>
    "test.keep_tensor_matmul"(%C) : (!dtensor.tensor<[%n_nat, %m_nat], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY-LABEL: func.func @tensor_matmul_function_ok(
// VERIFY-SAME: [[TN:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[TM:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[TK:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[AARG:%[0-9]+]]: !dtensor.tensor<{{\[}}[[TN]], [[TK]]], f32>
// VERIFY-SAME: [[BARG:%[0-9]+]]: !dtensor.tensor<{{\[}}[[TK]], [[TM]]], f32>
// VERIFY: [[TC:%[0-9]+]] = "dtensor.matmul"([[AARG]], [[BARG]]) : (!dtensor.tensor<{{\[}}[[TN]], [[TK]]], f32>, !dtensor.tensor<{{\[}}[[TK]], [[TM]]], f32>) -> !dtensor.tensor<{{\[}}[[TN]], [[TM]]], f32>
// VERIFY: "test.keep_tensor_matmul"([[TC]]) : (!dtensor.tensor<{{\[}}[[TN]], [[TM]]], f32>) -> ()

// -----

// Negative: direct tensor-dialect matmul rejects mismatched reduction dimensions.
builtin.module {
  func.func @tensor_matmul_function_bad_inner(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %k_nat : !dtensor.nat,
    %A : !dtensor.tensor<[%n_nat, %k_nat], f32>,
    %B : !dtensor.tensor<[%m_nat, %m_nat], f32>
  ) {
    // expected-error @below {{dtensor.matmul: expected SSA-identical inner dims}}
    %bad = "dtensor.matmul"(%A, %B)
      : (!dtensor.tensor<[%n_nat, %k_nat], f32>, !dtensor.tensor<[%m_nat, %m_nat], f32>)
     -> !dtensor.tensor<[%n_nat, %m_nat], f32>
    "test.keep_tensor_matmul_bad_inner"(%bad) : (!dtensor.tensor<[%n_nat, %m_nat], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// DIAG: dtensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)
