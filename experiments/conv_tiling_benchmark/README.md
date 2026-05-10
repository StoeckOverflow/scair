# conv_tiling_benchmark

This is a structural benchmark for full Conv2D-kernel reduction tiling. It shows
that ScaIR's product-provenance tiling story is not specific to matmul while
keeping the same kernel shape as `experiments/convolution_benchmark`.

The reduction domain is flattened to one product loop:

```text
R = Ci * Kh * Kw
```

Around that loop the benchmark keeps the real Conv2D structure: `N`, `Cout`,
`OH`, and `OW` outer loops, flat input/filter/output buffers, reinterpret casts,
loads, multiply-add accumulation, and final output stores.

The benchmark compares ordinary index arithmetic against explicit
`dtensor.nat.mul` provenance:

- `ordinary_conv_tail`: represents `Ci * Kh * Kw` with ordinary `arith.muli`;
  tiling keeps tail/min cleanup.
- `dependent_conv_guarded_tail_simplified`: uses the same guarded tiling style
  on the dependent Conv2D kernel, records the guarded artifact with a tail/min,
  then runs `dependent-tail-min-simplify` to remove that tail using explicit
  `dtensor.nat.mul` provenance.
- `dependent_conv_exact_dynamic`: represents `Ci * (Kh * Kw)` with explicit nat
  product provenance and `Kh * Kw : !dtensor.posnat`; exact tiling uses a
  dynamic proven-positive step and removes tail/min cleanup.
- `dependent_conv_exact_static_affine`: represents a specialized 3x3 Conv2D
  kernel as `Ci * 9` with an explicit static nat factor; exact tiling bridges
  to stock `affine.for` with static step 9.

The existing `experiments/convolution_benchmark` remains the executable
descriptor/lower-level-IR runtime benchmark. This benchmark uses the same Conv2D
memory and loop structure, but remains structural: its purpose is to inspect the
tiling transformation and tail-removal proof obligations.

## Run

```bash
experiments/conv_tiling_benchmark/build_conv_tiling_example.sh
```

Useful environment variables:

- `CONV_TILING_ROUTES`: comma-separated route list or `all`
- `CONV_TILING_ORDINARY_TILE`: ordinary-control tile size, default `5`
- `SCAIR_OPT`
- `OUT_DIR`

## Outputs

- `out/metrics.csv`
- `out/metrics.json`
- `out/summary.md`
- `out/route_manifest.md`
- `out/route_manifest.json`
- per-case `.input.mlir` and `.tiled.mlir` artifacts

## Interpretation

The expected result is structural:

- ordinary `arith.muli` product tiling keeps a tail bound in the full kernel;
- the dependent guarded route starts from the same conservative tail shape and
  removes the tail only after the proof-consuming simplifier runs;
- explicit dynamic `dtensor.nat.mul` product tiling is tail-free in the full
  kernel when the tile factor is proven positive;
- explicit static nat factors can bridge the full specialized 3x3 kernel back
  to stock affine-compatible loops.

The benchmark does not claim cache tuning, vectorization, padding/stride/dilation
coverage, or broad convolution runtime speedups.
