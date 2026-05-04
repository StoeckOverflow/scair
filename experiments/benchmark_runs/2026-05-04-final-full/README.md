# Benchmark Run: 2026-05-04 Final Full

Archived final experiment outputs copied from the live `experiments/*/out`
directories after the completed benchmark run.

Git commit recorded at archive time:

```text
c4a85258b7206f519bad42bdf581cf9c9d2084b1
```

Contents:

- `aggregate/`: copied from `experiments/out`, including aggregate metrics,
  summary, and environment snapshot.
- `per_family/`: one subdirectory per experiment family, preserving the full
  generated benchmark output contents, including metrics, summaries, raw timing
  text, generated MLIR, generated LLVM IR, object files, and executables.

The folder name avoids `out` so the repository-level ignore rule does not hide
the archived run.
