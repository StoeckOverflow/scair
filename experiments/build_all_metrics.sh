#!/usr/bin/env bash
set -euo pipefail

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

OUT_DIR="${OUT_DIR:-$SCAIR_ROOT/experiments/build_metrics}"
mkdir -p "$OUT_DIR"

# The per-family CSVs are expected to share one identical header so we can
# concatenate them directly. Family-specific metrics, if any, are appended after
# the shared core columns in COMMON_METRICS_HEADER.

SCRIPTS=(
  "$SCAIR_ROOT/experiments/type_polymorphism/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/memref_control_flow/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/semi_affine_indexing_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/matmul_checksum_benchmark/build_scair_example.sh"
)

METRIC_FILES=(
  "$SCAIR_ROOT/experiments/type_polymorphism/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/memref_control_flow/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/semi_affine_indexing_benchmark/build_scair/metrics.csv"
  "$SCAIR_ROOT/experiments/matmul_checksum_benchmark/build_scair/metrics.csv"
)

for script in "${SCRIPTS[@]}"; do
  echo "==> Running $(basename "$(dirname "$script")") metrics build"
  bash "$script"
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

with open(md_path, "w", encoding="utf-8") as out:
    out.write("# Uniform Experiment Metrics Summary\n\n")
    out.write("This summary keeps one core schema across all experiment families.\n\n")
    for family in sorted(groups):
        out.write(f"## {family}\n\n")
        out.write("| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | LLVM lines | Compile ms | Result | Expected | ns/iter | Notes |\n")
        out.write("| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: | --- |\n")
        for row in groups[family]:
            out.write(
                f"| {row['benchmark']} | {row['variant']} | {row['representation_group']} | "
                f"{row['build_status']} | {row['run_status']} | {row['source_ops_structural']} | "
                f"{row['source_func_defs']} | {row['source_block_args']} | {row['llvm_ir_lines']} | "
                f"{row['compile_ms']} | {row['result']} | {row['expected_result']} | "
                f"{row['runtime_ns_per_iter']} | {row['notes']} |\n"
            )
        out.write("\n")
PY

echo
echo "Aggregated metrics complete."
echo "Produced:"
echo "  $ALL_CSV"
echo "  $SUMMARY_MD"
