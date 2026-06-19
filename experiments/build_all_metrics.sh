#!/usr/bin/env bash
set -euo pipefail

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

OUT_DIR="${OUT_DIR:-$SCAIR_ROOT/experiments/out}"
mkdir -p "$OUT_DIR"

# Use thesis-facing defaults for the aggregate metrics run. Lightweight
# validation should override these explicitly at invocation time.
BENCH_WARMUP_REPS_DEFAULT="${BENCH_WARMUP_REPS:-5}"
BENCH_TIMING_REPS_DEFAULT="${BENCH_TIMING_REPS:-15}"
BENCH_CPU_PIN="${BENCH_CPU_PIN:-}"
export BENCH_CPU_PIN

# Family-specific iteration defaults balance reproducibility against practical
# end-to-end runtime for the full aggregate suite.
SEMI_AFFINE_ITERATIONS_DEFAULT="${SEMI_AFFINE_ITERATIONS:-1000}"
STRIDED_MATMUL_ITERATIONS_DEFAULT="${STRIDED_MATMUL_ITERATIONS:-200}"
CONVOLUTION_ITERATIONS_DEFAULT="${CONVOLUTION_ITERATIONS:-50}"
SKIP_BUILD="${SKIP_BUILD:-0}"

# The per-family CSVs are expected to share one identical header so we can
# concatenate them directly. Family-specific metrics, if any, are appended after
# the shared core columns in COMMON_METRICS_HEADER.

SCRIPTS=(
  "$SCAIR_ROOT/experiments/design_benchmarks/type_polymorphism/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/structural_benchmarks/semi_affine_indexing_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/structural_benchmarks/strided_matmul_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/structural_benchmarks/convolution_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/design_benchmarks/shape_reification_benchmark/build_shape_reification_example.sh"
)

METRIC_FILES=(
  "$SCAIR_ROOT/experiments/design_benchmarks/type_polymorphism/out/metrics.csv"
  "$SCAIR_ROOT/experiments/structural_benchmarks/semi_affine_indexing_benchmark/out/metrics.csv"
  "$SCAIR_ROOT/experiments/structural_benchmarks/strided_matmul_benchmark/out/metrics.csv"
  "$SCAIR_ROOT/experiments/structural_benchmarks/convolution_benchmark/out/metrics.csv"
)

COMMON_FAMILIES=(
  "design_benchmarks/type_polymorphism"
  "structural_benchmarks/semi_affine_indexing_benchmark"
  "structural_benchmarks/strided_matmul_benchmark"
  "structural_benchmarks/convolution_benchmark"
)

# Structural validation families intentionally keep family-specific schemas.
# Run them with the aggregate suite, but archive their outputs separately from
# the common-schema runtime CSV.
STRUCTURAL_FAMILIES=(
  "design_benchmarks/shape_reification_benchmark"
)

run_benchmark_script() {
  local script="$1"
  if [[ -n "$BENCH_CPU_PIN" ]]; then
    taskset -c "$BENCH_CPU_PIN" bash "$script"
  else
    bash "$script"
  fi
}

if [[ "$SKIP_BUILD" != "1" ]]; then
  for script in "${SCRIPTS[@]}"; do
    echo "==> Running $(basename "$(dirname "$script")") metrics build"
    case "$(basename "$(dirname "$script")")" in
      type_polymorphism)
        run_benchmark_script "$script"
        ;;
      semi_affine_indexing_benchmark)
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        ITERATIONS="$SEMI_AFFINE_ITERATIONS_DEFAULT" \
        run_benchmark_script "$script"
        ;;
      strided_matmul_benchmark)
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        ITERATIONS="$STRIDED_MATMUL_ITERATIONS_DEFAULT" \
        run_benchmark_script "$script"
        ;;
      convolution_benchmark)
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        ITERATIONS="$CONVOLUTION_ITERATIONS_DEFAULT" \
        run_benchmark_script "$script"
        ;;
      shape_reification_benchmark)
        run_benchmark_script "$script"
        ;;
      *)
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        run_benchmark_script "$script"
        ;;
    esac
  done
else
  echo "==> SKIP_BUILD=1: aggregating existing per-family metrics only"
fi

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

ENV_JSON="$OUT_DIR/env.json"
capture_env_snapshot "$ENV_JSON"

SUMMARY_MD="$OUT_DIR/summary.md"
python3 "$SCAIR_ROOT/experiments/summarize_results.py" "$ALL_CSV" "$SUMMARY_MD"

for family in "${COMMON_FAMILIES[@]}"; do
  family_out="$SCAIR_ROOT/experiments/$family/out"
  family_archive_dir="$OUT_DIR/$(dirname "$family")"
  family_name="$(basename "$family")"
  mkdir -p "$family_archive_dir"
  cp "$family_out/metrics.csv" "$family_archive_dir/$family_name.metrics.csv"
  if [[ -f "$family_out/summary.md" ]]; then
    cp "$family_out/summary.md" "$family_archive_dir/$family_name.summary.md"
  fi
done

STRUCTURAL_OUT_DIR="$OUT_DIR"
STRUCTURAL_MANIFEST="$OUT_DIR/structural_metrics_manifest.json"

{
  printf '[\n'
  for idx in "${!STRUCTURAL_FAMILIES[@]}"; do
    family="${STRUCTURAL_FAMILIES[$idx]}"
    family_out="$SCAIR_ROOT/experiments/$family/out"
    metrics_csv="$family_out/metrics.csv"
    metrics_json="$family_out/metrics.json"
    summary_md="$family_out/summary.md"
    route_manifest_md="$family_out/route_manifest.md"
    route_manifest_json="$family_out/route_manifest.json"

    require_file "$metrics_csv"
    require_file "$summary_md"

    family_archive_dir="$STRUCTURAL_OUT_DIR/$(dirname "$family")"
    family_name="$(basename "$family")"
    mkdir -p "$family_archive_dir"

    cp "$metrics_csv" "$family_archive_dir/$family_name.metrics.csv"
    cp "$summary_md" "$family_archive_dir/$family_name.summary.md"

    metrics_json_ref="null"
    if [[ -f "$metrics_json" ]]; then
      cp "$metrics_json" "$family_archive_dir/$family_name.metrics.json"
      metrics_json_ref="\"$family.metrics.json\""
    fi

    route_manifest_md_ref="null"
    if [[ -f "$route_manifest_md" ]]; then
      cp "$route_manifest_md" "$family_archive_dir/$family_name.route_manifest.md"
      route_manifest_md_ref="\"$family.route_manifest.md\""
    fi

    route_manifest_json_ref="null"
    if [[ -f "$route_manifest_json" ]]; then
      cp "$route_manifest_json" "$family_archive_dir/$family_name.route_manifest.json"
      route_manifest_json_ref="\"$family.route_manifest.json\""
    fi

    if [[ "$idx" -gt 0 ]]; then
      printf ',\n'
    fi
    printf '  {\n'
    printf '    "family": "%s",\n' "$family"
    printf '    "metrics_csv": "%s.metrics.csv",\n' "$family"
    printf '    "metrics_json": %s,\n' "$metrics_json_ref"
    printf '    "summary_md": "%s.summary.md",\n' "$family"
    printf '    "route_manifest_md": %s,\n' "$route_manifest_md_ref"
    printf '    "route_manifest_json": %s\n' "$route_manifest_json_ref"
    printf '  }'
  done
  printf '\n]\n'
} > "$STRUCTURAL_MANIFEST"

echo
echo "Aggregated metrics complete."
echo "Produced:"
echo "  $ALL_CSV"
echo "  $SUMMARY_MD"
echo "  $STRUCTURAL_MANIFEST"
echo "  $STRUCTURAL_OUT_DIR/"
