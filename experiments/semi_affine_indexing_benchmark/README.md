# semi_affine_indexing_benchmark

$$
\ell(i,j)=i\,s_0+j\,s_1
\qquad
0\leq i<M,\;0\leq j<N
$$

$$
A_{\ell(i,j)} = 1
$$

$$
\mathrm{checksum}
=
\sum_{i=0}^{M-1}
\sum_{j=0}^{N-1}
A_{\ell(i,j)}
=
MN
$$

Tests:
- supporting microbenchmark for semi-affine indexing through dynamic strides $s_0$ and $s_1$
- compares upstream MLIR, ScaIR baseline, and value-dependent `d_memref`/`d_affine` routes
- validates that fill and reduction over the same semi-affine layout preserve the expected checksum

Use in thesis:
- expressiveness and lowering evidence for dynamic-strided semi-affine layouts
- supporting runtime validation only; near-parity results should not be framed as a broad speedup claim
- useful for showing that value-dependent memory provenance composes with affine-style indexing

Metric interpretation / limitations:
- The benchmark reports a 256x1024 fill/reduction and should not be described as an 8x8 case.
- `scair_baseline` is the ScaIR dynamic memref baseline and intentionally does not carry dependent `d_memref` provenance.
- The runtime result validates the generated route and checksum, but the benchmark is not a broad speedup claim for semi-affine indexing.
