#!/usr/bin/env bash
set -euo pipefail

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

OUT_DIR="${OUT_DIR:-$SCAIR_ROOT/experiments/build_metrics}"
mkdir -p "$OUT_DIR"

# Use a stricter default measurement policy for the aggregate metrics run so
# the reported timings are more stable than the lightweight per-family defaults.
BENCH_WARMUP_REPS_DEFAULT="${BENCH_WARMUP_REPS:-3}"
BENCH_TIMING_REPS_DEFAULT="${BENCH_TIMING_REPS:-9}"

# Family-specific iteration defaults balance reproducibility against practical
# end-to-end runtime for the full aggregate suite.
TYPE_POLYMORPHISM_ITERATIONS_DEFAULT="${TYPE_POLYMORPHISM_ITERATIONS:-10000000}"
MEMREF_CONTROL_FLOW_ITERATIONS_DEFAULT="${MEMREF_CONTROL_FLOW_ITERATIONS:-20000}"
MEMREF_CONTROL_FLOW_RUNTIME_N_DEFAULT="${MEMREF_CONTROL_FLOW_RUNTIME_N:-64}"
SEMI_AFFINE_ITERATIONS_DEFAULT="${SEMI_AFFINE_ITERATIONS:-1000}"
MATMUL_CHECKSUM_ITERATIONS_DEFAULT="${MATMUL_CHECKSUM_ITERATIONS:-200}"
STRIDED_MATMUL_ITERATIONS_DEFAULT="${STRIDED_MATMUL_ITERATIONS:-200}"
CONVOLUTION_ITERATIONS_DEFAULT="${CONVOLUTION_ITERATIONS:-50}"

# The per-family CSVs are expected to share one identical header so we can
# concatenate them directly. Family-specific metrics, if any, are appended after
# the shared core columns in COMMON_METRICS_HEADER.

SCRIPTS=(
  "$SCAIR_ROOT/experiments/type_polymorphism/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/memref_control_flow/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/semi_affine_indexing_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/matmul_checksum_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/strided_matmul_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/convolution_benchmark/build_scair_example.sh"
)

METRIC_FILES=(
  "$SCAIR_ROOT/experiments/type_polymorphism/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/memref_control_flow/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/semi_affine_indexing_benchmark/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/matmul_checksum_benchmark/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/strided_matmul_benchmark/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/convolution_benchmark/build_scair/metrics.csv"
)

for script in "${SCRIPTS[@]}"; do
  echo "==> Running $(basename "$(dirname "$script")") metrics build"
  case "$(basename "$(dirname "$script")")" in
    type_polymorphism)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      ITERATIONS="$TYPE_POLYMORPHISM_ITERATIONS_DEFAULT" \
      bash "$script"
      ;;
    memref_control_flow)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      ITERATIONS="$MEMREF_CONTROL_FLOW_ITERATIONS_DEFAULT" \
      RUNTIME_N="$MEMREF_CONTROL_FLOW_RUNTIME_N_DEFAULT" \
      bash "$script"
      ;;
    semi_affine_indexing_benchmark)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      ITERATIONS="$SEMI_AFFINE_ITERATIONS_DEFAULT" \
      bash "$script"
      ;;
    matmul_checksum_benchmark)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      ITERATIONS="$MATMUL_CHECKSUM_ITERATIONS_DEFAULT" \
      bash "$script"
      ;;
    strided_matmul_benchmark)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      ITERATIONS="$STRIDED_MATMUL_ITERATIONS_DEFAULT" \
      bash "$script"
      ;;
    convolution_benchmark)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      ITERATIONS="$CONVOLUTION_ITERATIONS_DEFAULT" \
      bash "$script"
      ;;
    *)
      BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
      BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
      bash "$script"
      ;;
  esac
done

for metrics in "${METRIC_FILES[@]}"; do
  require_file "$metrics"
  header=$(head -n 1 "$metrics")
  if [[ "$header" != "$COMMON_METRICS_HEADER" ]]; then
    echo "error: unexpected metrics header in $metrics" >&2
    echo "got:      $header" >&2
    echo "expected: $COMMON_METRICS_HEADER" >&2
    exit 1
  fi
done

ALL_CSV="$OUT_DIR/all_metrics.csv"
printf '%s\n' "$COMMON_METRICS_HEADER" > "$ALL_CSV"
for metrics in "${METRIC_FILES[@]}"; do
  tail -n +2 "$metrics" >> "$ALL_CSV"
done

SUMMARY_MD="$OUT_DIR/summary.md"
python3 - "$ALL_CSV" "$SUMMARY_MD" <<'PY'
import csv
import sys
from collections import defaultdict

csv_path, md_path = sys.argv[1], sys.argv[2]
rows = list(csv.DictReader(open(csv_path, newline="", encoding="utf-8")))
groups = defaultdict(list)
for row in rows:
    groups[row["experiment_family"]].append(row)

variant_order = {
    "mlir_baseline": 0,
    "scair_baseline": 1,
    "debruijn": 1,
    "value_dependent": 2,
}

def rep_value(row):
    note = row["notes"]
    if note.startswith("selector="):
        return note
    if row["representation_group"] in {"mlir_baseline", "scair_baseline", "value_dependent"}:
        return ""
    return row["representation_group"]

with open(md_path, "w", encoding="utf-8") as out:
    out.write("# Uniform Experiment Metrics Summary\n\n")
    out.write("This summary keeps one core schema across all experiment families.\n\n")
    for family in sorted(groups):
        out.write(f"## {family}\n\n")
        out.write("| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |\n")
        out.write("| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |\n")
        sorted_rows = sorted(
            groups[family],
            key=lambda row: (
                row["benchmark"],
                variant_order.get(row["variant"], 99),
                rep_value(row),
            ),
        )
        for row in sorted_rows:
            out.write(
                f"| {row['benchmark']} | {row['variant']} | {rep_value(row)} | "
                f"{row['build_status']} | {row['run_status']} | {row['source_ops_structural']} | "
                f"{row['source_func_defs']} | {row['source_block_args']} | {row['lowered_mlir_lines']} | {row['llvm_ir_lines']} | "
                f"{row['compile_ms']} | {row['result']} | {row['expected_result']} | "
                f"{row['runtime_ns_per_iter']} |\n"
            )
        out.write("\n")
    out.write("## Metric Definitions\n\n")
    out.write("- `Benchmark`: benchmark or benchmark family member represented by the row.\n")
    out.write("- `Variant`: implementation route being compared, for example `mlir_baseline`, `scair_baseline`, `debruijn`, or `value_dependent`.\n")
    out.write("- `Rep`: representation-specific note for the row. For selector experiments this records the selector setting, such as `selector=0` or `selector=1`.\n")
    out.write("- `Build`: build outcome for the benchmark artifact. `ok` means the benchmark built successfully. `unsupported` means the pipeline failed or the route is not currently supported.\n")
    out.write("- `Run`: benchmark execution outcome. `ok` means the executable ran and produced timing/result data. `NA` means no run data was produced.\n")
    out.write("- `Structural ops`: total parsed IR operation nodes in the measured source IR. This is a parser-backed structural count, not a line count and not a regex/text estimate.\n")
    out.write("- `Func defs`: parsed count of function definition operations in the measured IR, currently `func.func` plus `llvm.func`.\n")
    out.write("- `Block args`: parsed count of SSA block arguments across all blocks in the measured IR.\n")
    out.write("- `MLIR LOC`: line count of the emitted lowered MLIR artifact on disk, measured with `wc -l`. This is a textual file metric taken after the MLIR file has been generated.\n")
    out.write("- `LLVM LOC`: line count of the emitted LLVM IR `.ll` artifact on disk, measured with `wc -l`. This is a textual file metric taken after the LLVM IR file has been generated.\n")
    out.write("- `Compile ms`: wall-clock build time for the benchmark pipeline, reported in milliseconds.\n")
    out.write("- `Result`: observed benchmark result value produced by the executable.\n")
    out.write("- `Expected`: expected benchmark result used as a correctness check.\n")
    out.write("- `ns/iter`: median runtime in nanoseconds per iteration across repeated benchmark runs.\n")
PY

echo
echo "Aggregated metrics complete."
echo "Produced:"
echo "  $ALL_CSV"
echo "  $SUMMARY_MD"
