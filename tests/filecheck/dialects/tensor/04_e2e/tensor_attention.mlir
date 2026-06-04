// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s | scair-opt --allow-unregistered-dialect --verify-diagnostics

// Transformer projection GEMM with head_dim = 64.
//
// Shapes:
//   BS = B * S                          (flatten batch and sequence)
//   H  = num_heads * head_dim           (hidden size)
//   head_dim = 64                       (constant, TensorCore-friendly)
//
// GEMM:
//   A : [BS, H]
//   W : [H,  H]
//   O = A * W : [BS, H]

func.func @tf_mha_projection_gemm(
    %B  : !dtensor.nat,        // batch size
    %S  : !dtensor.nat,        // sequence length
    %nh : !dtensor.nat         // num_heads (symbolic)
) {

  // head_dim = 64 is a very common transformer choice
  %hd64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat

  // BS = B * S  (flatten batch x sequence)
  %BS = "dtensor.nat.mul" (%B, %S) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  // H = num_heads * head_dim
  %H  = "dtensor.nat.mul" (%nh, %hd64) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  // A: [BS, H]  (activations)
  %A = "dtensor.empty" () : () -> !dtensor.tensor<[%BS, %H], f32>

  // W: [H, H]   (projection weights)
  %W = "dtensor.empty" () : () -> !dtensor.tensor<[%H, %H], f32>

  // O: [BS, H] = A x W
  %O = "dtensor.matmul" (%A, %W)
       : (!dtensor.tensor<[%BS, %H], f32>, !dtensor.tensor<[%H, %H], f32>)
         -> !dtensor.tensor<[%BS, %H], f32>

  func.return %O : !dtensor.tensor<[%BS, %H], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @tf_mha_projection_gemm(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat) {
// VERIFY:     %3 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// VERIFY:     %4 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:     %5 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:     %6 = "dtensor.empty"() : () -> !dtensor.tensor<[%4, %5], f32>
// VERIFY:     %7 = "dtensor.empty"() : () -> !dtensor.tensor<[%5, %5], f32>
// VERIFY:     %8 = "dtensor.matmul"(%6, %7) : (!dtensor.tensor<[%4, %5], f32>, !dtensor.tensor<[%5, %5], f32>) -> !dtensor.tensor<[%4, %5], f32>
// VERIFY:     func.return %8 : !dtensor.tensor<[%4, %5], f32>
// VERIFY:   }
// VERIFY: }
