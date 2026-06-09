// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s | scair-opt --allow-unregistered-dialect --verify-diagnostics

func.func @gemm_symbolic_factorized(
    %m      : !d_tensor.size,
    %nTiles : !d_tensor.size,
    %TN     : !d_tensor.size,
    %kTiles : !d_tensor.size,
    %TK     : !d_tensor.size
) {

  %N = "d_tensor.size.mul" (%nTiles, %TN) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %K = "d_tensor.size.mul" (%kTiles, %TK) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size

  %A = "d_tensor.empty" () : () -> !d_tensor.tensor<[%m, %K], f32>
  %B = "d_tensor.empty" () : () -> !d_tensor.tensor<[%K, %N], f32>
  %C = "d_tensor.matmul" (%A, %B)
       : (!d_tensor.tensor<[%m,%K], f32>, !d_tensor.tensor<[%K,%N], f32>)
         -> !d_tensor.tensor<[%m,%N], f32>
  func.return %C : !d_tensor.tensor<[%m,%N], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @gemm_symbolic_factorized(%0: !d_tensor.size, %1: !d_tensor.size, %2: !d_tensor.size, %3: !d_tensor.size, %4: !d_tensor.size) {
// VERIFY:     %5 = "d_tensor.size.mul"(%1, %2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// VERIFY:     %6 = "d_tensor.size.mul"(%3, %4) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// VERIFY:     %7 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %6], f32>
// VERIFY:     %8 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%6, %5], f32>
// VERIFY:     %9 = "d_tensor.matmul"(%7, %8) : (!d_tensor.tensor<[%0, %6], f32>, !d_tensor.tensor<[%6, %5], f32>) -> !d_tensor.tensor<[%0, %5], f32>
// VERIFY:     func.return %9 : !d_tensor.tensor<[%0, %5], f32>
// VERIFY:   }
// VERIFY: }
