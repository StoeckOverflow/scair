// Purpose: Kernel-shaped matmul coverage mirroring experiment IR with explicit dependent memref shape checks.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

// Positive: experiment-style factorized matmul kernel with matching A/B/C shapes.
builtin.module {
  func.func @matmul_full_factorized_tiling(
    %m0_nat : !dtensor.nat,
    %m1_nat : !dtensor.posnat,
    %n0_nat : !dtensor.nat,
    %n1_nat : !dtensor.posnat,
    %k0_nat : !dtensor.nat,
    %k1_nat : !dtensor.posnat,
    %Aflat : !d_memref.memref<[], f32>,
    %Bflat : !d_memref.memref<[], f32>,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %m_nat = "dtensor.nat.mul"(%m0_nat, %m1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %n_nat = "dtensor.nat.mul"(%n0_nat, %n1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index

    %A = d_memref.reinterpret_cast %Aflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%m_nat, %k_nat], f32,
             offset: 0, strides: [%k, %c1]>

    %B = d_memref.reinterpret_cast %Bflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%k_nat, %n_nat], f32,
             offset: 0, strides: [%n, %c1]>

    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%m_nat, %n_nat], f32,
             offset: 0, strides: [%n, %c1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%m_nat, %k_nat], f32, offset: 0, strides: [%k, %c1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %shape_ok = d_memref.cast %C
      : !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
     -> !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
    "test.keep_kernel_matmul"(%shape_ok) : (!d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>) -> ()
    "func.return"() : () -> ()
  }
}

// VERIFY-LABEL: func.func @matmul_full_factorized_tiling(
// VERIFY-SAME: [[M0:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[M1:%[0-9]+]]: !dtensor.posnat
// VERIFY-SAME: [[N0:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[N1:%[0-9]+]]: !dtensor.posnat
// VERIFY-SAME: [[K0:%[0-9]+]]: !dtensor.nat
// VERIFY-SAME: [[K1:%[0-9]+]]: !dtensor.posnat
// VERIFY-SAME: [[AFLAT:%[0-9]+]]: !d_memref.memref<[], f32>
// VERIFY-SAME: [[BFLAT:%[0-9]+]]: !d_memref.memref<[], f32>
// VERIFY-SAME: [[CFLAT:%[0-9]+]]: !d_memref.memref<[], f32>
// VERIFY: [[M:%[0-9]+]] = "dtensor.nat.mul"([[M0]], [[M1]]) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
// VERIFY: [[N:%[0-9]+]] = "dtensor.nat.mul"([[N0]], [[N1]]) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
// VERIFY: [[K:%[0-9]+]] = "dtensor.nat.mul"([[K0]], [[K1]]) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
// VERIFY: [[ONE:%[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// VERIFY: [[N_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[N]]) : (!dtensor.nat) -> index
// VERIFY: [[K_IDX:%[0-9]+]] = "dtensor.shape.to_index"([[K]]) : (!dtensor.nat) -> index
// VERIFY: [[A:%[0-9]+]] = d_memref.reinterpret_cast [[AFLAT]]
// VERIFY-NEXT: : !d_memref.memref<[], f32> to !d_memref.memref<{{\[}}[[M]], [[K]]], f32, offset: 0, strides: {{\[}}[[K_IDX]], [[ONE]]]>
// VERIFY: [[B:%[0-9]+]] = d_memref.reinterpret_cast [[BFLAT]]
// VERIFY-NEXT: : !d_memref.memref<[], f32> to !d_memref.memref<{{\[}}[[K]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]>
// VERIFY: [[C:%[0-9]+]] = d_memref.reinterpret_cast [[CFLAT]]
// VERIFY-NEXT: : !d_memref.memref<[], f32> to !d_memref.memref<{{\[}}[[M]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]>
// VERIFY: [[AVAL:%[0-9]+]] = d_memref.load [[A]][{{%[0-9]+}}, {{%[0-9]+}}] : !d_memref.memref<{{\[}}[[M]], [[K]]], f32, offset: 0, strides: {{\[}}[[K_IDX]], [[ONE]]]> -> f32
// VERIFY: [[BVAL:%[0-9]+]] = d_memref.load [[B]][{{%[0-9]+}}, {{%[0-9]+}}] : !d_memref.memref<{{\[}}[[K]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]> -> f32
// VERIFY: [[PROD:%[0-9]+]] = "arith.mulf"([[AVAL]], [[BVAL]])
// VERIFY: d_memref.store {{%[0-9]+}}, [[C]][{{%[0-9]+}}, {{%[0-9]+}}] : f32, !d_memref.memref<{{\[}}[[M]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]>
// VERIFY: [[SHAPE_OK:%[0-9]+]] = d_memref.cast [[C]] : !d_memref.memref<{{\[}}[[M]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]> -> !d_memref.memref<{{\[}}[[M]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]>
// VERIFY: "test.keep_kernel_matmul"([[SHAPE_OK]]) : (!d_memref.memref<{{\[}}[[M]], [[N]]], f32, offset: 0, strides: {{\[}}[[N_IDX]], [[ONE]]]>) -> ()

// -----

// Negative: same experiment-style matmul shape plumbing, but C is asserted as M x K instead of M x N.
builtin.module {
  func.func @matmul_full_factorized_tiling_bad_result(
    %m0_nat : !dtensor.nat,
    %m1_nat : !dtensor.posnat,
    %n0_nat : !dtensor.nat,
    %n1_nat : !dtensor.posnat,
    %k0_nat : !dtensor.nat,
    %k1_nat : !dtensor.posnat,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %m_nat = "dtensor.nat.mul"(%m0_nat, %m1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %n_nat = "dtensor.nat.mul"(%n0_nat, %n1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%m_nat, %n_nat], f32,
             offset: 0, strides: [%n, %c1]>
    // expected-error @below {{d_memref.cast: expected pairwise SSA-identical dims}}
    %bad = d_memref.cast %C
      : !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
     -> !d_memref.memref<[%m_nat, %k_nat], f32, offset: 0, strides: [%n, %c1]>
    "test.keep_kernel_matmul_bad"(%bad) : (!d_memref.memref<[%m_nat, %k_nat], f32, offset: 0, strides: [%n, %c1]>) -> ()
    "func.return"() : () -> ()
  }
}

// DIAG: "d_memref.cast"({{%[0-9]+}}) : (!d_memref.memref<{{\[}}%{{[0-9]+}}, %{{[0-9]+}}], f32, offset: 0, strides: {{\[}}%{{[0-9]+}}, %{{[0-9]+}}]>) -> !d_memref.memref<{{\[}}%{{[0-9]+}}, %{{[0-9]+}}], f32, offset: 0, strides: {{\[}}%{{[0-9]+}}, %{{[0-9]+}}]>
// DIAG: d_memref.cast: expected pairwise SSA-identical dims

// -----

// Negative: kernel memref cast rejects changed layout metadata even when dimensions match.
builtin.module {
  func.func @matmul_layout_mismatch(
    %m_nat : !dtensor.nat,
    %n_nat : !dtensor.nat,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%m_nat, %n_nat], f32,
             offset: 0, strides: [%n, %c1]>
    // expected-error @below {{d_memref.cast: expected identical layout metadata}}
    %bad = d_memref.cast %C
      : !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
     -> !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%m, %c1]>
    "test.keep_kernel_layout_bad"(%bad) : (!d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%m, %c1]>) -> ()
    "func.return"() : () -> ()
  }
}

// DIAG: d_memref.cast: expected identical layout metadata

// -----

// Negative: layout SSA parameters embedded in d_memref types must dominate their uses.
builtin.module {
  "test.region"() ({
  ^bb0:
    %m_nat = "dtensor.nat.param"() : () -> !dtensor.nat
    %n_nat = "dtensor.nat.param"() : () -> !dtensor.nat
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %stride = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    // expected-error @below {{ssa-dominance: value}}
    %bad = "test.bad_layout"() : () -> !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%stride, 1]>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// DIAG: ssa-dominance: value Value(index) does not dominate its use in op `test.bad_layout`
