# Tail/Min Simplifier Benchmark

This structural benchmark demonstrates a post-hoc optimization enabled by dependent nat provenance:

```text
ordinary guarded tiling:    min(tile + T, N)
dependent provenance:       N = N0 * T
tail/min simplifier:        min(tile + T, N) => tile + T
```

The benchmark is deliberately small. It does not claim runtime speedup. Its purpose is to show that
a conservative guarded tiler can emit tail code first, and a later ScaIR pass can remove that code
only when `arith.muli` provenance proves exact divisibility.

## Variants

- `stock_affine_guarded_tile`: emits stock `affine.for ... to min` from an ordinary product loop and
  then runs upstream `canonicalize,cse,affine-simplify-min-max`. The `to min` tail remains because
  the product is represented only as ordinary SSA arithmetic.
- `ordinary_d_affine_guarded_tile`: uses an ordinary `arith.muli` product with the same
  known-positive dynamic RHS used as the tile step. Cleanup runs, but `dependent-tail-min-simplify`
  does not; the generated `arith.minsi` tail guard remains.
- `dependent_guarded_tile_simplified`: uses the congruent dependent `arith.muli` product and
  runs `dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce`. The
  `arith.minsi` guard is removed.

## Metrics

The script records:

- `affine_min_count`
- `arith_minsi_count`
- `tail_guard_count`
- dynamic/static loop step counts
- `arith.muli` and `d_tensor.direct index` counts
- total operation count and MLIR LOC
- removed-op delta from the guarded form where applicable

Run:

```bash
experiments/tiling_benchmarks/tail_min_simplifier_benchmark/build_tail_min_simplifier_example.sh
```

Useful environment variables:

- `SCAIR_OPT`, default `out/tools/opt/launcher.dest/run`
- `LLVM_BUILD_DIR`, default `$HOME/dev/llvm-clean-build`
- `MLIR_OPT`, default `$LLVM_BUILD_DIR/bin/mlir-opt`
- `OUT_DIR`, default `experiments/tiling_benchmarks/tail_min_simplifier_benchmark/out`

Outputs are written under `experiments/tiling_benchmarks/tail_min_simplifier_benchmark/out/`. The
script also emits `route_manifest.md` and `route_manifest.json` documenting the structural roles of
each variant.
