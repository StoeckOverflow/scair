// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s | scair-opt --allow-unregistered-dialect --verify-diagnostics

func.func @gemm_symbolic_factorized(
    %m      : index,
    %nBlocks : index,
    %blockN  : index,
    %kBlocks : index,
    %blockK  : index
) {

  %N = "arith.muli" (%nBlocks, %blockN) : (index, index) -> index
  %K = "arith.muli" (%kBlocks, %blockK) : (index, index) -> index

  %A = "d_tensor.empty" () : () -> !d_tensor.tensor<[%m, %K], f32>
  %B = "d_tensor.empty" () : () -> !d_tensor.tensor<[%K, %N], f32>
  %C = "d_tensor.matmul" (%A, %B)
       : (!d_tensor.tensor<[%m,%K], f32>, !d_tensor.tensor<[%K,%N], f32>)
         -> !d_tensor.tensor<[%m,%N], f32>
  func.return %C : !d_tensor.tensor<[%m,%N], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @gemm_symbolic_factorized(%0: index, %1: index, %2: index, %3: index, %4: index) {
// VERIFY:     %5 = "arith.muli"(%1, %2) {{.*}} : (index, index) -> index
// VERIFY:     %6 = "arith.muli"(%3, %4) {{.*}} : (index, index) -> index
// VERIFY:     %7 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %6], f32>
// VERIFY:     %8 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%6, %5], f32>
// VERIFY:     %9 = "d_tensor.matmul"(%7, %8) : (!d_tensor.tensor<[%0, %6], f32>, !d_tensor.tensor<[%6, %5], f32>) -> !d_tensor.tensor<[%0, %5], f32>
// VERIFY:     func.return %9 : !d_tensor.tensor<[%0, %5], f32>
// VERIFY:   }
// VERIFY: }
