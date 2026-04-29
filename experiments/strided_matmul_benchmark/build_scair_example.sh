#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-1}"
GEMM_SIZE_SET="${GEMM_SIZE_SET:-128x128x128,256x256x256,512x512x512}"
GEMM_INCLUDE_1024="${GEMM_INCLUDE_1024:-0}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="$BIN_DIR/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
COMPILE_TIME_BENCH="$SCAIR_ROOT/experiments/compile_time_benchmark.sh"

MLIR_SRC="${MLIR_SRC:-$EXAMPLE_DIR/matmul_kernel_mlir_baseline.mlir}"
SCAIR_BASELINE_SRC="${SCAIR_BASELINE_SRC:-$EXAMPLE_DIR/matmul_kernel_scair_baseline.mlir}"
SCAIR_VALUE_DEP_SRC="${SCAIR_VALUE_DEP_SRC:-$EXAMPLE_DIR/matmul_kernel_scair_dmemref.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver_mlir.c}"
SCAIR_BASELINE_DRIVER_SRC="${SCAIR_BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline.c}"
SCAIR_VALUE_DEP_DRIVER_SRC="${SCAIR_VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver_scair.c}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"

MLIR_PIPELINE="--lower-affine --convert-scf-to-cf --expand-strided-metadata --finalize-memref-to-llvm --convert-arith-to-llvm --convert-index-to-llvm --convert-cf-to-llvm --convert-func-to-llvm --reconcile-unrealized-casts"
SCAIR_BASELINE_PIPELINE="lower-dynamic-memref-to-llvm-baseline,convert-func-to-llvm,convert-llvm-export-abi"
SCAIR_VALUE_DEP_PIPELINE="lower-dmemref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
COMPILER_FLAGS="-O2"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$COMPILE_TIME_BENCH"
require_file "$MLIR_SRC"
require_file "$SCAIR_BASELINE_SRC"
require_file "$SCAIR_VALUE_DEP_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$SCAIR_BASELINE_DRIVER_SRC"
require_file "$SCAIR_VALUE_DEP_DRIVER_SRC"

normalize_size_set() {
  local sizes="$1"
  if [[ "$GEMM_INCLUDE_1024" == "1" && "$sizes" != *"1024x1024x1024"* ]]; then
    sizes="${sizes},1024x1024x1024"
  fi
  echo "$sizes"
}

size_tag() {
  echo "$1" | tr 'x' '_'
}

measure_compile_timing() {
  local tool="$1"
  local input="$2"
  local pipeline="$3"
  local out_json="$4"
  "$COMPILE_TIME_BENCH" "$tool" "$input" "$out_json" "$pipeline"
}

build_mlir_route() {
  local src="$1"
  local obj_out="$2"
  local lowered_mlir_out="$3"
  local llvm_ir_out="$4"

  "$MLIR_OPT" "$src" \
    --lower-affine \
    --convert-scf-to-cf \
    --expand-strided-metadata \
    --finalize-memref-to-llvm \
    --convert-arith-to-llvm \
    --convert-index-to-llvm \
    --convert-cf-to-llvm \
    --convert-func-to-llvm \
    --reconcile-unrealized-casts \
    > "$lowered_mlir_out"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_out" > "$llvm_ir_out"
  "$CC" -O2 -x ir "$llvm_ir_out" -c -o "$obj_out"
}

build_scair_route() {
  local src="$1"
  local pipeline="$2"
  local obj_out="$3"
  local lowered_mlir_out="$4"
  local llvm_ir_out="$5"

  "$SCAIR_OPT" -s "$src" --passes "$pipeline" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' \
    > "$lowered_mlir_out"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_out" > "$llvm_ir_out"
  "$CC" -O2 -x ir "$llvm_ir_out" -c -o "$obj_out"
}

run_variant_for_size() {
  local exe="$1"
  local output_txt="$2"
  local n="$3"
  local m="$4"
  local k="$5"

  set +e
  run_benchmark_repeated "$output_txt" "$exe" "$n" "$m" "$k" "$ITERATIONS"
  local status=$?
  set -e

  if [[ $status -ne 0 ]]; then
    echo "error: GEMM benchmark run failed for size n=$n m=$m k=$k using $exe" >&2
    exit $status
  fi
}

append_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local variant="$3"
  local representation="$4"
  local src="$5"
  local lowered_mlir="$6"
  local llvm_ir="$7"
  local output_txt="$8"
  local compile_ms="$9"
  local timing_json="${10}"
  local size_descriptor="${11}"
  local notes="${12}"
  local raw_timings_path

  raw_timings_path="$(metric_field raw_timings_path "$output_txt")"
  if [[ "$(metric_field run_status "$output_txt")" == "ok" ]]; then
    require_nonempty_file "$raw_timings_path"
  fi

  append_metrics_csv_row \
    "$metrics_csv" \
    "strided_matmul_benchmark" \
    "matmul_strided" \
    "$variant" \
    "$representation" \
    "ok" \
    "$(metric_field run_status "$output_txt")" \
    "$(file_metric bytes "$src")" \
    "$(file_metric lines "$src")" \
    "$(count_ops "$src")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(count_alloc_ops "$src")" \
    "$(count_source_reinterpret_cast_ops "$src")" \
    "$(count_source_subview_ops "$src")" \
    "$(count_source_extract_strided_metadata_ops "$src")" \
    "$(count_source_memref_load_ops "$src")" \
    "$(count_source_memref_store_ops "$src")" \
    "$(count_source_dmemref_load_ops "$src")" \
    "$(count_source_dmemref_store_ops "$src")" \
    "$(count_func_defs "$lowered_mlir")" \
    "$(count_ops "$lowered_mlir")" \
    "$(count_ops_structural "$lowered_mlir")" \
    "$(file_metric lines "$lowered_mlir")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(count_llvm_calls "$llvm_ir")" \
    "$compile_ms" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "$notes" \
    "$(count_source_helpers "$src")" \
    "0" \
    "0" \
    "NA" \
    "NA" \
    "gemm" \
    "$size_descriptor" \
    "$variant" \
    "NA" \
    "$(json_stage_elapsed_ms "$timing_json" verification)" \
    "$(json_stage_elapsed_ms "$timing_json" lowering)" \
    "$(json_top_level_field "$timing_json" total_ms)" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "$(metric_field runtime_iqr_ns_per_iter "$output_txt")" \
    "$(metric_field benchmark_repetitions "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$([[ "$(metric_field run_status "$output_txt")" == "ok" ]] && echo ok || echo NA)" \
    "$COMPILER_FLAGS" \
    "$GIT_COMMIT" \
    "$RUN_DATE" \
    "$MACHINE_ID" \
    "$ENV_PATH" \
    "$raw_timings_path"

  append_summary_row \
    "$summary_md" \
    "matmul_strided" \
    "$variant" \
    "$size_descriptor" \
    "ok" \
    "$(metric_field run_status "$output_txt")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$lowered_mlir")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(json_top_level_field "$timing_json" total_ms)" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

echo "==> Building upstream MLIR GEMM baseline"
MLIR_TIMING_JSON="$OUT_DIR/matmul_mlir_baseline.compile_timing.json"
measure_compile_timing "$MLIR_OPT" "$MLIR_SRC" "$MLIR_PIPELINE" "$MLIR_TIMING_JSON"
mlir_start=$(now_ns)
build_mlir_route "$MLIR_SRC" "$OUT_DIR/matmul_mlir_baseline.o" "$OUT_DIR/matmul_mlir_baseline.llvm.mlir" "$OUT_DIR/matmul_mlir_baseline.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"strided_matmul\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$MLIR_DRIVER_SRC" \
  "$OUT_DIR/matmul_mlir_baseline.o" \
  -o "$OUT_DIR/matmul_mlir_baseline_exec"
mlir_end=$(now_ns)
MLIR_COMPILE_MS="$(format_ms "$mlir_start" "$mlir_end")"

echo "==> Building ScaIR dynamic GEMM baseline"
SCAIR_BASELINE_TIMING_JSON="$OUT_DIR/matmul_scair_baseline.compile_timing.json"
measure_compile_timing "$SCAIR_OPT" "$SCAIR_BASELINE_SRC" "$SCAIR_BASELINE_PIPELINE" "$SCAIR_BASELINE_TIMING_JSON"
baseline_start=$(now_ns)
build_scair_route "$SCAIR_BASELINE_SRC" "$SCAIR_BASELINE_PIPELINE" "$OUT_DIR/matmul_scair_baseline.o" "$OUT_DIR/matmul_scair_baseline.llvm.mlir" "$OUT_DIR/matmul_scair_baseline.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"strided_matmul\"" \
  -DVARIANT_LABEL="\"scair_baseline\"" \
  "$SCAIR_BASELINE_DRIVER_SRC" \
  "$OUT_DIR/matmul_scair_baseline.o" \
  -o "$OUT_DIR/matmul_scair_baseline_exec"
baseline_end=$(now_ns)
SCAIR_BASELINE_COMPILE_MS="$(format_ms "$baseline_start" "$baseline_end")"

echo "==> Building ScaIR dependent GEMM route"
SCAIR_VALUE_DEP_TIMING_JSON="$OUT_DIR/matmul_scair_dmemref.compile_timing.json"
measure_compile_timing "$SCAIR_OPT" "$SCAIR_VALUE_DEP_SRC" "$SCAIR_VALUE_DEP_PIPELINE" "$SCAIR_VALUE_DEP_TIMING_JSON"
value_start=$(now_ns)
build_scair_route "$SCAIR_VALUE_DEP_SRC" "$SCAIR_VALUE_DEP_PIPELINE" "$OUT_DIR/matmul_scair_dmemref.o" "$OUT_DIR/matmul_scair_dmemref.llvm.mlir" "$OUT_DIR/matmul_scair_dmemref.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"strided_matmul\"" \
  -DVARIANT_LABEL="\"scair_dmemref\"" \
  "$SCAIR_VALUE_DEP_DRIVER_SRC" \
  "$OUT_DIR/matmul_scair_dmemref.o" \
  -o "$OUT_DIR/matmul_scair_dmemref_exec"
value_end=$(now_ns)
SCAIR_VALUE_DEP_COMPILE_MS="$(format_ms "$value_start" "$value_end")"

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Strided Matmul Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

IFS=',' read -r -a GEMM_SIZES <<<"$(normalize_size_set "$GEMM_SIZE_SET")"
for dims in "${GEMM_SIZES[@]}"; do
  IFS='x' read -r n m k <<<"$dims"
  size_descriptor="n=${n};m=${m};k=${k}"
  tag="$(size_tag "$dims")"

  run_variant_for_size "$OUT_DIR/matmul_mlir_baseline_exec" "$OUT_DIR/matmul_mlir_baseline_${tag}_output.txt" "$n" "$m" "$k"
  append_row \
    "$SUMMARY_CSV" "$SUMMARY_MD" \
    "mlir_baseline" "mlir_baseline" \
    "$MLIR_SRC" \
    "$OUT_DIR/matmul_mlir_baseline.llvm.mlir" \
    "$OUT_DIR/matmul_mlir_baseline.ll" \
    "$OUT_DIR/matmul_mlir_baseline_${tag}_output.txt" \
    "$MLIR_COMPILE_MS" \
    "$MLIR_TIMING_JSON" \
    "$size_descriptor" \
    "upstream MLIR dynamic baseline with memref.reinterpret_cast; timed region includes output reset plus kernel execution"

  run_variant_for_size "$OUT_DIR/matmul_scair_baseline_exec" "$OUT_DIR/matmul_scair_baseline_${tag}_output.txt" "$n" "$m" "$k"
  append_row \
    "$SUMMARY_CSV" "$SUMMARY_MD" \
    "scair_baseline" "scair_baseline" \
    "$SCAIR_BASELINE_SRC" \
    "$OUT_DIR/matmul_scair_baseline.llvm.mlir" \
    "$OUT_DIR/matmul_scair_baseline.ll" \
    "$OUT_DIR/matmul_scair_baseline_${tag}_output.txt" \
    "$SCAIR_BASELINE_COMPILE_MS" \
    "$SCAIR_BASELINE_TIMING_JSON" \
    "$size_descriptor" \
    "ScaIR dynamic baseline lowered through lower-dynamic-memref-to-llvm-baseline; timed region includes output reset plus kernel execution"

  run_variant_for_size "$OUT_DIR/matmul_scair_dmemref_exec" "$OUT_DIR/matmul_scair_dmemref_${tag}_output.txt" "$n" "$m" "$k"
  append_row \
    "$SUMMARY_CSV" "$SUMMARY_MD" \
    "scair_dmemref" "value_dependent" \
    "$SCAIR_VALUE_DEP_SRC" \
    "$OUT_DIR/matmul_scair_dmemref.llvm.mlir" \
    "$OUT_DIR/matmul_scair_dmemref.ll" \
    "$OUT_DIR/matmul_scair_dmemref_${tag}_output.txt" \
    "$SCAIR_VALUE_DEP_COMPILE_MS" \
    "$SCAIR_VALUE_DEP_TIMING_JSON" \
    "$size_descriptor" \
    "ScaIR dependent d_memref route with refined reinterpret-cast views; timed region includes output reset plus kernel execution"
done

echo
echo "Strided matmul build complete."
echo "Produced:"
echo "  $OUT_DIR/matmul_mlir_baseline_exec"
echo "  $OUT_DIR/matmul_scair_baseline_exec"
echo "  $OUT_DIR/matmul_scair_dmemref_exec"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
