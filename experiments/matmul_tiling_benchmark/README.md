# matmul_tiling_benchmark

$$
K = K_0K_1
$$

$$
C_{i,j}
=
\sum_{p=0}^{K-1}
A_{i,p}\,B_{p,j}
\qquad
0\leq i<M,\;0\leq j<N
$$

Tests:
- supporting reduction-tiling benchmark for direct product provenance $K=K_0K_1$
- compares ordinary dynamic $K$ arithmetic with value-dependent $dtensor.nat.mul$ provenance

Use in thesis:
- structural/code-generation evidence with supporting runtime validation
- not a broad matmul speedup claim or proof of general product solving
