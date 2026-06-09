// Purpose: Matmul kernel coverage: the strided experiment IR plus direct d_tensor.matmul verifier cases.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

builtin.module {
  func.func @matmul_strided(
    %n_size : !d_tensor.size,
    %m_size : !d_tensor.size,
    %k_size : !d_tensor.size,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %A : !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
    %B : !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
    %C : !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m_size) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }

  func.func @call_matmul_strided_good(
    %n_size : !d_tensor.size,
    %m_size : !d_tensor.size,
    %k_size : !d_tensor.size,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %A : !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
    %B : !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
    %C : !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
  ) {
    %kernel = func.constant @matmul_strided : (!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index,
         !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    "func.call_indirect"(%kernel, %n_size, %m_size, %k_size, %a_stride0, %a_stride1, %b_stride0, %b_stride1, %c_stride0, %c_stride1, %A, %B, %C)
      : ((!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index,
          !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
          !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
          !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> (),
         !d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index,
         !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    "func.return"() : () -> ()
  }

  func.func @call_matmul_strided_bad(
    %n_size : !d_tensor.size,
    %m_size : !d_tensor.size,
    %k_size : !d_tensor.size,
    %a_stride0 : index,
    %a_stride1 : index,
    %b_stride0 : index,
    %b_stride1 : index,
    %c_stride0 : index,
    %c_stride1 : index,
    %A : !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
    %B : !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
    %C_bad : !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>
  ) {
    %kernel = func.constant @matmul_strided : (!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index,
         !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    // expected-error @below {{func.call_indirect: argument types}}
    "func.call_indirect"(%kernel, %n_size, %m_size, %k_size, %a_stride0, %a_stride1, %b_stride0, %b_stride1, %c_stride0, %c_stride1, %A, %B, %C_bad)
      : ((!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index,
          !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
          !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
          !d_memref.memref<[%n_size, %m_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> (),
         !d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index,
         !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%a_stride0, %a_stride1]>,
         !d_memref.memref<[%k_size, %m_size], f32, offset: 0, strides: [%b_stride0, %b_stride1]>,
         !d_memref.memref<[%n_size, %k_size], f32, offset: 0, strides: [%c_stride0, %c_stride1]>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY: "func.func"() <{sym_name = "matmul_strided", function_type = (!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[N:[0-9]+]], %[[K:[0-9]+]]], f32, offset: 0, strides: [%[[AS0:[0-9]+]], %[[AS1:[0-9]+]]]>{{,}} !d_memref.memref<[%[[K]], %[[M:[0-9]+]]], f32, offset: 0, strides: [%[[BS0:[0-9]+]], %[[BS1:[0-9]+]]]>{{,}} !d_memref.memref<[%[[N]], %[[M]]], f32, offset: 0, strides: [%[[CS0:[0-9]+]], %[[CS1:[0-9]+]]]>) -> ()}>
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%[[N]], %[[K]]], f32, offset: 0, strides: [%[[AS0]], %[[AS1]]]>, index, index) -> f32
// VERIFY: "d_memref.load"({{.*}}) : (!d_memref.memref<[%[[K]], %[[M]]], f32, offset: 0, strides: [%[[BS0]], %[[BS1]]]>, index, index) -> f32
// VERIFY: "d_memref.store"({{.*}}) : (f32, !d_memref.memref<[%[[N]], %[[M]]], f32, offset: 0, strides: [%[[CS0]], %[[CS1]]]>, index, index) -> ()
// VERIFY: "func.func"() <{sym_name = "call_matmul_strided_good"
// VERIFY: %[[GOOD_KERNEL:[0-9]+]] = "func.constant"() <{value = @matmul_strided}> : () -> (!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[GOOD_N:[0-9]+]], %[[GOOD_K:[0-9]+]]], f32, offset: 0, strides: [%[[GOOD_AS0:[0-9]+]], %[[GOOD_AS1:[0-9]+]]]>{{,}} !d_memref.memref<[%[[GOOD_K]], %[[GOOD_M:[0-9]+]]], f32, offset: 0, strides: [%[[GOOD_BS0:[0-9]+]], %[[GOOD_BS1:[0-9]+]]]>{{,}} !d_memref.memref<[%[[GOOD_N]], %[[GOOD_M]]], f32, offset: 0, strides: [%[[GOOD_CS0:[0-9]+]], %[[GOOD_CS1:[0-9]+]]]>) -> ()
// VERIFY: "func.call_indirect"(%[[GOOD_KERNEL]], %[[GOOD_N]], %[[GOOD_M]], %[[GOOD_K]], %[[GOOD_AS0]], %[[GOOD_AS1]], %[[GOOD_BS0]], %[[GOOD_BS1]], %[[GOOD_CS0]], %[[GOOD_CS1]], %0, %1, %2) : ((!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[GOOD_N]], %[[GOOD_K]]], f32, offset: 0, strides: [%[[GOOD_AS0]], %[[GOOD_AS1]]]>{{,}} !d_memref.memref<[%[[GOOD_K]], %[[GOOD_M]]], f32, offset: 0, strides: [%[[GOOD_BS0]], %[[GOOD_BS1]]]>{{,}} !d_memref.memref<[%[[GOOD_N]], %[[GOOD_M]]], f32, offset: 0, strides: [%[[GOOD_CS0]], %[[GOOD_CS1]]]>) -> (), !d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[GOOD_N]], %[[GOOD_K]]], f32, offset: 0, strides: [%[[GOOD_AS0]], %[[GOOD_AS1]]]>{{,}} !d_memref.memref<[%[[GOOD_K]], %[[GOOD_M]]], f32, offset: 0, strides: [%[[GOOD_BS0]], %[[GOOD_BS1]]]>{{,}} !d_memref.memref<[%[[GOOD_N]], %[[GOOD_M]]], f32, offset: 0, strides: [%[[GOOD_CS0]], %[[GOOD_CS1]]]>) -> ()
// VERIFY: "func.func"() <{sym_name = "call_matmul_strided_bad"
// VERIFY: %[[BAD_KERNEL:[0-9]+]] = "func.constant"() <{value = @matmul_strided}> : () -> (!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[BAD_N:[0-9]+]], %[[BAD_K:[0-9]+]]], f32, offset: 0, strides: [%[[BAD_AS0:[0-9]+]], %[[BAD_AS1:[0-9]+]]]>{{,}} !d_memref.memref<[%[[BAD_K]], %[[BAD_M:[0-9]+]]], f32, offset: 0, strides: [%[[BAD_BS0:[0-9]+]], %[[BAD_BS1:[0-9]+]]]>{{,}} !d_memref.memref<[%[[BAD_N]], %[[BAD_M]]], f32, offset: 0, strides: [%[[BAD_CS0:[0-9]+]], %[[BAD_CS1:[0-9]+]]]>) -> ()
// VERIFY: "func.call_indirect"(%[[BAD_KERNEL]], %[[BAD_N]], %[[BAD_M]], %[[BAD_K]], %[[BAD_AS0]], %[[BAD_AS1]], %[[BAD_BS0]], %[[BAD_BS1]], %[[BAD_CS0]], %[[BAD_CS1]], %0, %1, %2) : ((!d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[BAD_N]], %[[BAD_K]]], f32, offset: 0, strides: [%[[BAD_AS0]], %[[BAD_AS1]]]>{{,}} !d_memref.memref<[%[[BAD_K]], %[[BAD_M]]], f32, offset: 0, strides: [%[[BAD_BS0]], %[[BAD_BS1]]]>{{,}} !d_memref.memref<[%[[BAD_N]], %[[BAD_M]]], f32, offset: 0, strides: [%[[BAD_CS0]], %[[BAD_CS1]]]>) -> (), !d_tensor.size, !d_tensor.size, !d_tensor.size, index, index, index, index, index, index, !d_memref.memref<[%[[BAD_N]], %[[BAD_K]]], f32, offset: 0, strides: [%[[BAD_AS0]], %[[BAD_AS1]]]>{{,}} !d_memref.memref<[%[[BAD_K]], %[[BAD_M]]], f32, offset: 0, strides: [%[[BAD_BS0]], %[[BAD_BS1]]]>{{,}} !d_memref.memref<[%[[BAD_N]], %[[BAD_K]]], f32, offset: 0, strides: [%[[BAD_CS0]], %[[BAD_CS1]]]>) -> ()
// DIAG: func.call_indirect: argument types
// DIAG-SAME: do not match callee input types

// -----

// Positive: direct tensor-dialect matmul function.
builtin.module {
  func.func @tensor_matmul_function_ok(
    %n_size : !d_tensor.size,
    %m_size : !d_tensor.size,
    %k_size : !d_tensor.size,
    %A : !d_tensor.tensor<[%n_size, %k_size], f32>,
    %B : !d_tensor.tensor<[%k_size, %m_size], f32>
  ) {
    %C = "d_tensor.matmul"(%A, %B)
      : (!d_tensor.tensor<[%n_size, %k_size], f32>, !d_tensor.tensor<[%k_size, %m_size], f32>)
     -> !d_tensor.tensor<[%n_size, %m_size], f32>
    "test.keep_tensor_matmul"(%C) : (!d_tensor.tensor<[%n_size, %m_size], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY-LABEL: func.func @tensor_matmul_function_ok(
// VERIFY-SAME: [[TN:%[0-9]+]]: !d_tensor.size
// VERIFY-SAME: [[TM:%[0-9]+]]: !d_tensor.size
// VERIFY-SAME: [[TK:%[0-9]+]]: !d_tensor.size
// VERIFY-SAME: [[AARG:%[0-9]+]]: !d_tensor.tensor<{{\[}}[[TN]], [[TK]]], f32>
// VERIFY-SAME: [[BARG:%[0-9]+]]: !d_tensor.tensor<{{\[}}[[TK]], [[TM]]], f32>
// VERIFY: [[TC:%[0-9]+]] = "d_tensor.matmul"([[AARG]], [[BARG]]) : (!d_tensor.tensor<{{\[}}[[TN]], [[TK]]], f32>, !d_tensor.tensor<{{\[}}[[TK]], [[TM]]], f32>) -> !d_tensor.tensor<{{\[}}[[TN]], [[TM]]], f32>
// VERIFY: "test.keep_tensor_matmul"([[TC]]) : (!d_tensor.tensor<{{\[}}[[TN]], [[TM]]], f32>) -> ()

// -----

// Negative: direct tensor-dialect matmul rejects mismatched reduction dimensions.
builtin.module {
  func.func @tensor_matmul_function_bad_inner(
    %n_size : !d_tensor.size,
    %m_size : !d_tensor.size,
    %k_size : !d_tensor.size,
    %A : !d_tensor.tensor<[%n_size, %k_size], f32>,
    %B : !d_tensor.tensor<[%m_size, %m_size], f32>
  ) {
    // expected-error @below {{d_tensor.matmul: expected SSA-identical inner dims}}
    %bad = "d_tensor.matmul"(%A, %B)
      : (!d_tensor.tensor<[%n_size, %k_size], f32>, !d_tensor.tensor<[%m_size, %m_size], f32>)
     -> !d_tensor.tensor<[%n_size, %m_size], f32>
    "test.keep_tensor_matmul_bad_inner"(%bad) : (!d_tensor.tensor<[%n_size, %m_size], f32>) -> ()
    "func.return"() : () -> ()
  }
}

// DIAG: d_tensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)
