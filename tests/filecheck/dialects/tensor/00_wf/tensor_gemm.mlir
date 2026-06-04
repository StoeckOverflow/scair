// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s | scair-opt --allow-unregistered-dialect --verify-diagnostics

func.func @gemm_symbolic_factorized(
    %m      : !dtensor.nat,
    %nTiles : !dtensor.nat,
    %TN     : !dtensor.nat,
    %kTiles : !dtensor.nat,
    %TK     : !dtensor.nat
) {

  %N = "dtensor.nat.mul" (%nTiles, %TN) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %K = "dtensor.nat.mul" (%kTiles, %TK) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %A = "dtensor.empty" () : () -> !dtensor.tensor<[%m, %K], f32>
  %B = "dtensor.empty" () : () -> !dtensor.tensor<[%K, %N], f32>
  %C = "dtensor.matmul" (%A, %B)
       : (!dtensor.tensor<[%m,%K], f32>, !dtensor.tensor<[%K,%N], f32>)
         -> !dtensor.tensor<[%m,%N], f32>
  func.return %C : !dtensor.tensor<[%m,%N], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @gemm_symbolic_factorized(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat, %3: !dtensor.nat, %4: !dtensor.nat) {
// VERIFY:     %5 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:     %6 = "dtensor.nat.mul"(%3, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:     %7 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %6], f32>
// VERIFY:     %8 = "dtensor.empty"() : () -> !dtensor.tensor<[%6, %5], f32>
// VERIFY:     %9 = "dtensor.matmul"(%7, %8) : (!dtensor.tensor<[%0, %6], f32>, !dtensor.tensor<[%6, %5], f32>) -> !dtensor.tensor<[%0, %5], f32>
// VERIFY:     func.return %9 : !dtensor.tensor<[%0, %5], f32>
// VERIFY:   }
// VERIFY: }
