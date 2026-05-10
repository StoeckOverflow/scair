# Tail/Min Simplifier Benchmark

This structural benchmark demonstrates a post-hoc optimization enabled by
dependent nat provenance:

```text
ordinary guarded tiling:    min(tile + T, N)
dependent provenance:       N = N0 * T
tail/min simplifier:        min(tile + T, N) => tile + T
```

The benchmark is deliberately small. It does not claim runtime speedup. Its
purpose is to show that a conservative guarded tiler can emit tail code first,
and a later ScaIR pass can remove that code only when `dtensor.nat.mul`
provenance proves exact divisibility.

## Variants

- `ordinary_d_affine_guarded_tile`: uses ordinary `arith.muli` product
  structure. The `dependent-tail-min-simplify` pass runs, but the tail min must
  remain because no dependent proof is present.
- `dependent_guarded_tile_no_simplify`: uses `dtensor.nat.mul` but does not run
  the simplifier. This shows the conservative guarded form before proof
  consumption.
- `dependent_guarded_tile_simplified`: runs
  `dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce`.
  The `arith.minsi` guard is removed.
- `dependent_exact_tile_reference`: runs the direct exact tiler. This is only a
  reference endpoint; the benchmark’s main claim is the post-hoc simplifier.
- `stock_affine_guarded_tile`: emits stock `affine.for ... to min` from an
  ordinary product loop and then runs upstream
  `canonicalize,cse,affine-simplify-min-max`. This control illustrates that
  upstream affine cleanup does not see ScaIR’s dependent nat proof when the
  product is represented only as ordinary SSA arithmetic.

## Metrics

The script records:

- `affine_min_count`
- `arith_minsi_count`
- `tail_guard_count`
- dynamic/static loop step counts
- `dtensor.nat.mul` and `dtensor.shape.to_index` counts
- total operation count and MLIR LOC
- removed-op delta from the guarded form where applicable

Run:

```bash
experiments/tail_min_simplifier_benchmark/build_tail_min_simplifier_example.sh
```

Useful environment variables:

- `SCAIR_OPT`, default `out/tools/opt/launcher.dest/run`
- `LLVM_BUILD_DIR`, default `$HOME/dev/llvm-clean-build`
- `MLIR_OPT`, default `$LLVM_BUILD_DIR/bin/mlir-opt`
- `OUT_DIR`, default `experiments/tail_min_simplifier_benchmark/out`

Outputs are written under `experiments/tail_min_simplifier_benchmark/out/`.
The script also emits `route_manifest.md` and `route_manifest.json` documenting
the structural roles of each variant.
