# attention_mha_benchmark

$$
H = \mathrm{heads}\cdot D
$$

$$
S_{b,h,i,j}
= \frac{1}{\sqrt{D}}
  \sum_{d=0}^{D-1}
  Q_{b,i,hD+d}\,K_{b,j,hD+d}
$$

$$
P_{b,h,i,j}
=
\frac{\exp\!\left(S_{b,h,i,j}-\max_{0\leq t< L} S_{b,h,i,t}\right)}
     {\sum_{t=0}^{L-1}\exp\!\left(S_{b,h,i,t}-\max_{0\leq u< L} S_{b,h,i,u}\right)}
$$

$$
O_{b,i,hD+d}
=
\sum_{j=0}^{L-1}
P_{b,h,i,j}\,V_{b,j,hD+d}
$$

Tests:
- supporting attention/MHA route with explicit $H=\mathrm{heads}\cdot D$ factorization
- structural/code-generation evidence for preserving head and head-dimension information

Variant notes:
- `mlir_baseline`: upstream MLIR baseline route
- `scair_baseline`: ScaIR dynamic memref baseline without dependent provenance
- `value_dependent`: ScaIR dependent route with factorization-aware structure
- deterministic value inputs are chosen so the numeric output checksum is non-negative

Use in thesis:
- supporting evidence only; not a headline runtime benchmark
- does not isolate full model performance or general attention optimization

Metric interpretation / limitations:
- `scair_baseline` is the ScaIR dynamic memref baseline and intentionally does not carry dependent `dtensor.nat.mul` provenance.
- The value-dependent route demonstrates preserved head/head-dimension provenance and factorization-aware generated structure; it should not be framed as a broad attention-performance result.
- `affine_cleanup_present`, `factorized_tile_count`, and `tail_free_factorized` are artifact checks over generated `.tiled.mlir`, not semantic proofs.
