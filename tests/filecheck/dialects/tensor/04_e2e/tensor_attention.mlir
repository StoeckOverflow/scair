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
    %B  : !d_tensor.nat,        // batch size
    %S  : !d_tensor.nat,        // sequence length
    %nh : !d_tensor.nat         // num_heads (symbolic)
) {

  // head_dim = 64 is a very common transformer choice
  %hd64 = "d_tensor.nat.const"() <{value = 64 : i32}> : () -> !d_tensor.nat

  // BS = B * S  (flatten batch x sequence)
  %BS = "d_tensor.nat.mul" (%B, %S) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  // H = num_heads * head_dim
  %H  = "d_tensor.nat.mul" (%nh, %hd64) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  // A: [BS, H]  (activations)
  %A = "d_tensor.empty" () : () -> !d_tensor.tensor<[%BS, %H], f32>

  // W: [H, H]   (projection weights)
  %W = "d_tensor.empty" () : () -> !d_tensor.tensor<[%H, %H], f32>

  // O: [BS, H] = A x W
  %O = "d_tensor.matmul" (%A, %W)
       : (!d_tensor.tensor<[%BS, %H], f32>, !d_tensor.tensor<[%H, %H], f32>)
         -> !d_tensor.tensor<[%BS, %H], f32>

  func.return %O : !d_tensor.tensor<[%BS, %H], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @tf_mha_projection_gemm(%0: !d_tensor.nat, %1: !d_tensor.nat, %2: !d_tensor.nat) {
// VERIFY:     %3 = "d_tensor.nat.const"() <{value = 64 : i32}> : () -> !d_tensor.nat
// VERIFY:     %4 = "d_tensor.nat.mul"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:     %5 = "d_tensor.nat.mul"(%2, %3) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:     %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%4, %5], f32>
// VERIFY:     %7 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%5, %5], f32>
// VERIFY:     %8 = "d_tensor.matmul"(%6, %7) : (!d_tensor.tensor<[%4, %5], f32>, !d_tensor.tensor<[%5, %5], f32>) -> !d_tensor.tensor<[%4, %5], f32>
// VERIFY:     func.return %8 : !d_tensor.tensor<[%4, %5], f32>
// VERIFY:   }
// VERIFY: }
