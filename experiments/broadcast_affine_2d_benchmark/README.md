# broadcast_affine_2d_benchmark

$$
K = K_0K_1
$$

$$
Y_{bK_1+j}
=
X_{bK_1+j}\,\mathrm{scale}_j+\mathrm{bias}_j
\qquad
0\leq b<K_0,\;0\leq j<K_1
$$

$$
\mathrm{checksum}
=
\sum_{b=0}^{K_0-1}
\sum_{j=0}^{K_1-1}
Y_{bK_1+j}
$$

Tests:
- supporting microbenchmark for direct product provenance $K=K_0K_1$
- compares conservative dynamic tail control with a rectangular $K_0\times K_1$ loop structure

Variant notes:
- `mlir_baseline`: upstream MLIR baseline route
- `scair_baseline`: ScaIR dynamic memref baseline without dependent provenance
- `value_dependent`: ScaIR dependent route exposing the factorized rectangular loop structure
- deterministic input values are chosen so the numeric result/checksum is non-negative

Use in thesis:
- cache/control-heavy supporting evidence only
- not a full BatchNorm benchmark and not a general runtime-speedup claim
