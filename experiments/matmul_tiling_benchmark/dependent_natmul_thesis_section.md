# Dependent NatMul Proofs for Exact Matmul Tiling

This section describes the matmul tiling experiment used to evaluate ScaIR's
value-dependent natural-number provenance. The benchmark computes

```text
C[i, j] = sum_{p = 0}^{K - 1} A[i, p] * B[p, j]
K = K0 * K1
```

The ordinary affine/control route materializes `K` as an index product:

```mlir
%k = arith.muli %k0, %k1 : index
```

When this loop is tiled by `K1`, the tiler cannot use the operational
`arith.muli` as a divisibility proof. It therefore preserves a tail bound:

```mlir
affine.for %tile = ... to ... step 64 iter_args(...) {
  affine.for %p = #map(%tile) to min #map2(%tile)[%k] step 1 iter_args(...) {
    ...
  }
}
```

The value-dependent route instead carries the product as a typed natural-number
construction:

```mlir
%k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat)
%k = "dtensor.shape.to_index"(%k_nat)
```

The exact tiler recognizes that the loop upper bound came from
`dtensor.nat.mul(K0, K1)`. It tiles the reduction dimension by the RHS factor
`K1`, so the outer tile loop enumerates exact blocks of size `K1`. This removes
the need for an `affine.min`, `d_affine.min`, or `arith.minsi` tail guard:

```mlir
%k1 = "dtensor.shape.to_index"(%k1_nat)
d_affine.for %tile = #map(%c0) to #map(%k) step %k1 : index iter_args(...) {
  %tile_end = "arith.addi"(%tile, %k1) : (index, index) -> index
  d_affine.for %p = #map(%tile) to #map(%tile_end) step 1 : i32 iter_args(...) {
    ...
  }
}
```

The dynamic `arith.addi` remains in this benchmark because `K1` is a runtime
natural value. The important claim is narrower: the dependent representation
proves that every tile is full, so the generated loop does not need tail/min
cleanup. In the separate static `nat.const` path, the tile end can also be
encoded as an affine constant offset such as `d0 + 8`, making the result
stock-affine-compatible for upstream normalization and unrolling.

## Static Stock-Affine Bridge Route

The benchmark also contains a static bridge variant,
`value_dependent_static_affine_compatible_unroll`. This route keeps the proof
story separate from the dynamic staging route:

```text
dtensor.nat.mul(K0, nat.const K1)
-> dependent exact tiling
-> d_affine-to-affine-compatible
-> upstream mlir-opt normalize / unroll
```

Because the RHS factor is `nat.const`, the exact tiler can emit a static tile
step and an affine constant-offset tile end:

```mlir
#map1 = affine_map<(d0) -> (d0 + K1)>
affine.for %tile = ... step K1 {
  affine.for %p = #map(%tile) to #map1(%tile) step 1 iter_args(...) {
    ...
  }
}
```

The stock MLIR experiment uses `affine-loop-unroll{unroll-factor=4}` by default
and records `full_unroll_k1=ok` separately when unrolling by the full static
tile size also succeeds. The executable artifact is built from a proof-erased
version of the already-bridged affine IR: the nat proof ops are rewritten to
ordinary index constants/multiplication only after the dependent tiler and stock
affine checks have consumed the proof.

## Benchmark Result

Run command:

```bash
experiments/matmul_tiling_benchmark/build_scair_example.sh
```

Run metadata:

- Date: `2026-05-09T09:28:17Z`
- Compiler flags: `-O2`
- Repetitions: `15`
- Metrics: `experiments/matmul_tiling_benchmark/out/metrics.csv`
- Summary: `experiments/matmul_tiling_benchmark/out/summary.md`

| Size | Variant | Median ns/iter | IQR | MLIR LOC | LLVM LOC | Tail/min cleanup | Stock affine checks | Proof |
|---|---|---:|---:|---:|---:|---|---|---|
| `M=128,N=128,K0=12,K1=64,K=768` | `mlir_baseline` | 6,143,987 | 687,249 | 94 | 121 | yes, outer affine min context | n/a | none |
| `M=128,N=128,K0=12,K1=64,K=768` | `mlir_affine_k_tile_reference` | 4,359,695 | 447,066 | 103 | 134 | yes, `to min` | n/a | none |
| `M=128,N=128,K0=12,K1=64,K=768` | `ordinary_scair_k_tile_with_tail` | 6,452,013 | 906,259 | 83 | 107 | yes, affine min | n/a | none |
| `M=128,N=128,K0=12,K1=64,K=768` | `value_dependent_exact_tile` | 5,811,364 | 595,532 | 57 | 78 | no | n/a | `dtensor.nat.mul` |
| `M=128,N=128,K0=12,K1=64,K=768` | `value_dependent_static_affine_compatible_unroll` | 6,398,277 | 994,710 | 118 | 134 | no | parse/canonicalize/normalize/unroll-4 ok; full K1 ok | `dtensor.nat.mul` + `nat.const` |
| `M=128,N=128,K0=16,K1=32,K=512` | `mlir_baseline` | 4,443,437 | 285,818 | 94 | 121 | yes, outer affine min context | n/a | none |
| `M=128,N=128,K0=16,K1=32,K=512` | `mlir_affine_k_tile_reference` | 3,309,478 | 543,044 | 103 | 134 | yes, `to min` | n/a | none |
| `M=128,N=128,K0=16,K1=32,K=512` | `ordinary_scair_k_tile_with_tail` | 4,546,133 | 844,338 | 83 | 107 | yes, affine min | n/a | none |
| `M=128,N=128,K0=16,K1=32,K=512` | `value_dependent_exact_tile` | 4,461,868 | 572,703 | 57 | 78 | no | n/a | `dtensor.nat.mul` |
| `M=128,N=128,K0=16,K1=32,K=512` | `value_dependent_static_affine_compatible_unroll` | 4,271,579 | 450,508 | 118 | 134 | no | parse/canonicalize/normalize/unroll-4 ok; full K1 ok | `dtensor.nat.mul` + `nat.const` |
| `M=256,N=128,K0=12,K1=64,K=768` | `mlir_baseline` | 11,477,774 | 860,621 | 94 | 121 | yes, outer affine min context | n/a | none |
| `M=256,N=128,K0=12,K1=64,K=768` | `mlir_affine_k_tile_reference` | 7,804,936 | 761,823 | 103 | 134 | yes, `to min` | n/a | none |
| `M=256,N=128,K0=12,K1=64,K=768` | `ordinary_scair_k_tile_with_tail` | 12,081,867 | 1,141,984 | 83 | 107 | yes, affine min | n/a | none |
| `M=256,N=128,K0=12,K1=64,K=768` | `value_dependent_exact_tile` | 12,067,515 | 702,056 | 57 | 78 | no | n/a | `dtensor.nat.mul` |
| `M=256,N=128,K0=12,K1=64,K=768` | `value_dependent_static_affine_compatible_unroll` | 11,946,297 | 626,572 | 118 | 134 | no | parse/canonicalize/normalize/unroll-4 ok; full K1 ok | `dtensor.nat.mul` + `nat.const` |

## Interpretation

The central result is structural rather than a broad matmul performance claim:
the value-dependent route is the only route that carries an explicit
`dtensor.nat.mul` proof and the only route that removes tail/min cleanup for the
reduction tile. This supports the thesis claim that value-dependent types can
make divisibility information available before lowering, where the compiler can
use it directly instead of rediscovering it from weak index arithmetic.

For downstream MLIR compatibility, the static bridge route is the key artifact:
it demonstrates that once `K1` is statically known, the same proof can be
lowered into stock `affine.for` structure accepted by upstream parse,
canonicalization, loop normalization, and loop unrolling without tail cleanup.

The runtime rows are useful as correctness and code-generation validation. They
show that the generated kernels run and produce the expected results, but this
benchmark should not be framed as showing that ScaIR currently outperforms
stock MLIR matmul. The stock MLIR K-tile reference is faster in these runs, while
the dependent route produces smaller emitted IR and proves tail freedom.

## Limitations

- The dynamic exact-tile route still uses `d_affine` and a dynamic step `%k1`.
  This is intentionally a ScaIR staging result, not yet a fully stock-affine
  loop form.
- Dependence legality for general parallelization/vectorization is not proven by
  the nat product alone; those optimizations need separate memory/dependence
  reasoning.
- The static `nat.const` bridge is the stock-affine-compatible thesis artifact
  for upstream `mlir-opt` normalization/unroll experiments. This dynamic matmul
  benchmark demonstrates the exact-divisibility proof on a matrix kernel.
- The static bridge route should not be interpreted as a vectorization/GPU
  result. It stops at stock affine normalization/unroll plus ordinary LLVM
  lowering for runtime validation.
