# attention_mha_benchmark

Archived supporting experiment. This benchmark is preserved because it shows
that `H = heads * head_dim` provenance can guide exact hidden-dimension tiling
in an MHA-shaped kernel. It is not part of the active thesis-facing aggregate
suite because the MLIR baseline does not tile the same loop target, so it is not
a clean apples-to-apples comparison.

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
- runtime checksum/result validation for the selected deterministic input

Variant notes:
- `mlir_baseline`: upstream MLIR baseline route
- `scair_baseline`: ScaIR dynamic memref baseline without dependent provenance
- `value_dependent_exact_tile`: ScaIR dependent route with an exact tile over the flattened hidden dimension
- `value_dependent`: compatibility route name; thesis-facing exact-tiling runs should use `value_dependent_exact_tile`
- deterministic value inputs are chosen so the numeric output checksum is non-negative

Use in thesis:
- supporting evidence only; not a headline runtime benchmark
- does not isolate full model performance or general attention optimization
- exact-tiling claim: ScaIR derives a dynamic step from `hidden = heads * head_dim` provenance for the flattened hidden loop and avoids the cleanup structure that an affine tiled loop would need without that proof
- positivity claim: the value-dependent route treats `heads` and `head_dim` as
  `!dtensor.posnat`, so the dynamic exact-tile step is checked by the same
  positivity discipline used by the matmul and affine tiling benchmarks

Metric interpretation / limitations:
- `scair_baseline` is the ScaIR dynamic memref baseline and intentionally does not carry dependent `dtensor.nat.mul` provenance.
- In the current MLIR source, upstream `affine-loop-tile` tiles outer affine bands; it does not isolate the same flattened hidden loop that the ScaIR exact-tile pass rewrites. Use `tile_loop` before making same-loop comparisons.
- The value-dependent route demonstrates preserved head/head-dimension provenance and exact tiling for the flattened hidden loop; it should not be framed as a broad attention-performance result.
- The value-dependent route runs `validate-d-affine-dynamic-steps` before
  lowering; `canonicalize-dtensor-nat-products` is used as IR cleanup, while
  legality comes from explicit `dtensor.nat.mul` provenance plus `!dtensor.posnat`.
- The separate `dependent-natmul-loop-factorization` pass rewrites a flat nat-mul loop into two unit-step factor loops and should not be used as same-step tiling evidence.
- `loop_transform`, `tile_loop`, `shared_tile_size`, `tile_size_source`, `dynamic_step_present`, `tail_cleanup_present`, and `exact_divisibility_proof` are route notes derived from build-script arguments and generated IR artifact checks.
- `affine_cleanup_present`, `factorized_tile_count`, and `tail_free_factorized` are legacy artifact checks over generated `.tiled.mlir`; the semantic exact-divisibility argument comes from `dtensor.nat.mul`, not from these regex notes alone.

Run:

```bash
ITERATIONS=1 \
ATTENTION_MHA_ROUTES=value_dependent_exact_tile \
experiments/archive/attention_mha_benchmark/build_scair_example.sh
```

Useful environment variables:
- `ATTENTION_MHA_ROUTES`: comma-separated route list or `all`
- `ATTENTION_MHA_TILE_SIZE`, default `64`
- `ITERATIONS`, `LLVM_BUILD_DIR`, `CC`, `OUT_DIR`

Outputs:
- `out/metrics.csv`
- `out/summary.md`
- route-local MLIR/LLVM artifacts, executables, output logs, and raw timings
