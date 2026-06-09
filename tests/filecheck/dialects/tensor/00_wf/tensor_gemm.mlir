// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s | scair-opt --allow-unregistered-dialect --verify-diagnostics

func.func @gemm_symbolic_factorized(
    %m      : !d_tensor.nat,
    %nTiles : !d_tensor.nat,
    %TN     : !d_tensor.nat,
    %kTiles : !d_tensor.nat,
    %TK     : !d_tensor.nat
) {

  %N = "d_tensor.nat.mul" (%nTiles, %TN) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %K = "d_tensor.nat.mul" (%kTiles, %TK) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  %A = "d_tensor.empty" () : () -> !d_tensor.tensor<[%m, %K], f32>
  %B = "d_tensor.empty" () : () -> !d_tensor.tensor<[%K, %N], f32>
  %C = "d_tensor.matmul" (%A, %B)
       : (!d_tensor.tensor<[%m,%K], f32>, !d_tensor.tensor<[%K,%N], f32>)
         -> !d_tensor.tensor<[%m,%N], f32>
  func.return %C : !d_tensor.tensor<[%m,%N], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @gemm_symbolic_factorized(%0: !d_tensor.nat, %1: !d_tensor.nat, %2: !d_tensor.nat, %3: !d_tensor.nat, %4: !d_tensor.nat) {
// VERIFY:     %5 = "d_tensor.nat.mul"(%1, %2) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:     %6 = "d_tensor.nat.mul"(%3, %4) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:     %7 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %6], f32>
// VERIFY:     %8 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%6, %5], f32>
// VERIFY:     %9 = "d_tensor.matmul"(%7, %8) : (!d_tensor.tensor<[%0, %6], f32>, !d_tensor.tensor<[%6, %5], f32>) -> !d_tensor.tensor<[%0, %5], f32>
// VERIFY:     func.return %9 : !d_tensor.tensor<[%0, %5], f32>
// VERIFY:   }
// VERIFY: }
