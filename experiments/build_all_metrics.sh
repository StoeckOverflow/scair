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
TYPE_POLYMORPHISM_ITERATIONS_DEFAULT="${TYPE_POLYMORPHISM_ITERATIONS:-10000000}"
SEMI_AFFINE_ITERATIONS_DEFAULT="${SEMI_AFFINE_ITERATIONS:-1000}"
STRIDED_MATMUL_ITERATIONS_DEFAULT="${STRIDED_MATMUL_ITERATIONS:-200}"
CONVOLUTION_ITERATIONS_DEFAULT="${CONVOLUTION_ITERATIONS:-50}"
ATTENTION_MHA_ITERATIONS_DEFAULT="${ATTENTION_MHA_ITERATIONS:-100}"
MATMUL_TILING_ITERATIONS_DEFAULT="${MATMUL_TILING_ITERATIONS:-100}"
MATMUL_TILING_PROFILE_DEFAULT="${MATMUL_TILING_PROFILE:-default}"
MATMUL_TILING_TILE_POLICY_DEFAULT="${MATMUL_TILING_TILE_POLICY:-fixed32}"
MATMUL_TILING_TILE_SIZE_SET_DEFAULT="${MATMUL_TILING_TILE_SIZE_SET:-}"
MATMUL_TILING_DEFAULT_SIZE_SET="128x128x12x64,128x128x16x32,256x128x12x64"
MATMUL_TILING_CACHE_CONTROL_SIZE_SET="8x8x4096x3,8x8x4096x5,8x8x4096x7,8x8x4096x8,16x16x2048x3,16x16x2048x5,16x16x2048x7,16x16x1024x16,32x16x1024x8"
MATMUL_TILING_CACHE_SWEEP_SIZE_SET="${MATMUL_TILING_CACHE_SWEEP_SIZE_SET:-$MATMUL_TILING_CACHE_CONTROL_SIZE_SET}"
if [[ -n "${MATMUL_TILING_SIZE_SET:-}" ]]; then
  MATMUL_TILING_SIZE_SET_DEFAULT="$MATMUL_TILING_SIZE_SET"
elif [[ "$MATMUL_TILING_PROFILE_DEFAULT" == "cache_control" ]]; then
  MATMUL_TILING_SIZE_SET_DEFAULT="$MATMUL_TILING_CACHE_CONTROL_SIZE_SET"
  MATMUL_TILING_ITERATIONS_DEFAULT="${MATMUL_TILING_ITERATIONS:-1000}"
  MATMUL_TILING_TILE_POLICY_DEFAULT="${MATMUL_TILING_TILE_POLICY:-inner_factor}"
elif [[ "$MATMUL_TILING_PROFILE_DEFAULT" == "cache_sweep" ]]; then
  MATMUL_TILING_SIZE_SET_DEFAULT="$MATMUL_TILING_CACHE_SWEEP_SIZE_SET"
  MATMUL_TILING_ITERATIONS_DEFAULT="${MATMUL_TILING_ITERATIONS:-1000}"
  MATMUL_TILING_TILE_SIZE_SET_DEFAULT="${MATMUL_TILING_TILE_SIZE_SET:-8,16,32,64,128}"
else
  MATMUL_TILING_SIZE_SET_DEFAULT="$MATMUL_TILING_DEFAULT_SIZE_SET"
fi
SKIP_BUILD="${SKIP_BUILD:-0}"

# The per-family CSVs are expected to share one identical header so we can
# concatenate them directly. Family-specific metrics, if any, are appended after
# the shared core columns in COMMON_METRICS_HEADER.

SCRIPTS=(
  "$SCAIR_ROOT/experiments/type_polymorphism/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/semi_affine_indexing_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/strided_matmul_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/convolution_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/attention_mha_benchmark/build_scair_example.sh"
  "$SCAIR_ROOT/experiments/matmul_tiling_benchmark/build_scair_example.sh"
)

METRIC_FILES=(
  "$SCAIR_ROOT/experiments/type_polymorphism/out/metrics.csv"
  "$SCAIR_ROOT/experiments/semi_affine_indexing_benchmark/out/metrics.csv"
  "$SCAIR_ROOT/experiments/strided_matmul_benchmark/out/metrics.csv"
  "$SCAIR_ROOT/experiments/convolution_benchmark/out/metrics.csv"
  "$SCAIR_ROOT/experiments/attention_mha_benchmark/out/metrics.csv"
  "$SCAIR_ROOT/experiments/matmul_tiling_benchmark/out/metrics.csv"
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
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        ITERATIONS="$TYPE_POLYMORPHISM_ITERATIONS_DEFAULT" \
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
      attention_mha_benchmark)
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        ITERATIONS="$ATTENTION_MHA_ITERATIONS_DEFAULT" \
        run_benchmark_script "$script"
        ;;
      matmul_tiling_benchmark)
        BENCH_WARMUP_REPS="$BENCH_WARMUP_REPS_DEFAULT" \
        BENCH_TIMING_REPS="$BENCH_TIMING_REPS_DEFAULT" \
        ITERATIONS="$MATMUL_TILING_ITERATIONS_DEFAULT" \
        MATMUL_TILING_SIZE_SET="$MATMUL_TILING_SIZE_SET_DEFAULT" \
        MATMUL_TILING_PROFILE="$MATMUL_TILING_PROFILE_DEFAULT" \
        MATMUL_TILING_TILE_POLICY="$MATMUL_TILING_TILE_POLICY_DEFAULT" \
        MATMUL_TILING_TILE_SIZE_SET="$MATMUL_TILING_TILE_SIZE_SET_DEFAULT" \
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

echo
echo "Aggregated metrics complete."
echo "Produced:"
echo "  $ALL_CSV"
echo "  $SUMMARY_MD"
