# blocked_pack_unpack_benchmark

$$
M = M_oT_M,\qquad N = N_oT_N
$$

$$
D_{m_o,n_o,m_i,n_i}
=
S_{m_oT_M+m_i,\;n_oT_N+n_i}
\qquad
0\leq m_o<M_o,\;0\leq n_o<N_o,\;0\leq m_i<T_M,\;0\leq n_i<T_N
$$

$$
\mathrm{checksum}
=
\sum_{m_o=0}^{M_o-1}
\sum_{n_o=0}^{N_o-1}
\sum_{m_i=0}^{T_M-1}
\sum_{n_i=0}^{T_N-1}
D_{m_o,n_o,m_i,n_i}
$$

Tests:
- pack-only supporting microbenchmark for direct product provenance $M=M_oT_M$ and $N=N_oT_N$
- compares conservative dynamic tail/min logic with value-dependent `dtensor.nat.mul` provenance
- value-dependent route exposes a rectangular $M_o\times N_o\times T_M\times T_N$ loop structure

Variant notes:
- `mlir_baseline`: upstream MLIR baseline route
- `scair_baseline`: ScaIR dynamic memref baseline without dependent provenance
- `value_dependent`: ScaIR dependent route exposing rectangular product-provenance loops
- deterministic input values are chosen so the numeric result/checksum is non-negative

Use in thesis:
- structural/code-generation evidence with supporting runtime validation
- inspect generated IR artifacts for baseline tail/min logic and dependent rectangular factorized loops
- runtime should be described as near parity to modest improvement, not a general pack/unpack speedup claim
- benchmark is pack-only; it does not include unpack, static-oracle variants, or general product solving
