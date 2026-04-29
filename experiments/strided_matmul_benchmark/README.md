# strided_matmul_benchmark

$$
C[i\,s^C_0+j\,s^C_1]
=
\sum_{p=0}^{K-1}
A[i\,s^A_0+p\,s^A_1]\,
B[p\,s^B_0+j\,s^B_1]
\qquad
0\leq i<N,\;0\leq j<M
$$

Tests:
- core strided GEMM runtime and lowering evidence over selected sizes
- compares upstream MLIR, ScaIR dynamic, and value-dependent routes

Use in thesis:
- supports selected-kernel execution and overhead claims
- not factorization-aware tiling evidence and not a broad GEMM optimization claim
