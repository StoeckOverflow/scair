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
    %B  : index,        // batch size
    %S  : index,        // sequence length
    %nh : index         // num_heads (symbolic)
) {

  // head_dim = 64 is a very common transformer choice
  %hd64 = "arith.constant"() <{value = 64 : index}> : () -> index

  // BS = B * S  (flatten batch x sequence)
  %BS = "arith.muli" (%B, %S) : (index, index) -> index

  // H = num_heads * head_dim
  %H  = "arith.muli" (%nh, %hd64) : (index, index) -> index

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
// VERIFY:   func.func @tf_mha_projection_gemm(%0: index, %1: index, %2: index) {
// VERIFY:     %3 = "arith.constant"() <{value = 64 : index}> : () -> index
// VERIFY:     %4 = "arith.muli"(%0, %1) {{.*}} : (index, index) -> index
// VERIFY:     %5 = "arith.muli"(%2, %3) {{.*}} : (index, index) -> index
// VERIFY:     %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%4, %5], f32>
// VERIFY:     %7 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%5, %5], f32>
// VERIFY:     %8 = "d_tensor.matmul"(%6, %7) : (!d_tensor.tensor<[%4, %5], f32>, !d_tensor.tensor<[%5, %5], f32>) -> !d_tensor.tensor<[%4, %5], f32>
// VERIFY:     func.return %8 : !d_tensor.tensor<[%4, %5], f32>
// VERIFY:   }
// VERIFY: }
