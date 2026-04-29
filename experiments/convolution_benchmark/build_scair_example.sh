#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-1}"
CONV_SIZE_SET="${CONV_SIZE_SET:-1x3x32x32x16x3x3,1x16x64x64x32x3x3,1x64x224x224x64x3x3}"
CONV_LARGE_OUTPUT_ELEMENTS_THRESHOLD="${CONV_LARGE_OUTPUT_ELEMENTS_THRESHOLD:-1000000}"
CONV_LARGE_ITERATIONS="${CONV_LARGE_ITERATIONS:-${CONVOLUTION_LARGE_ITERATIONS:-1}}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="${BIN_DIR}/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
COMPILE_TIME_BENCH="$SCAIR_ROOT/experiments/compile_time_benchmark.sh"

CONV_MLIR_SRC="${CONV_MLIR_SRC:-$EXAMPLE_DIR/conv2d_kernel.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"
CONV_BASELINE_SRC="${CONV_BASELINE_SRC:-$EXAMPLE_DIR/conv2d_kernel_scair_baseline.mlir}"
BASELINE_DRIVER_SRC="${BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline_bare.c}"
CONV_VALUE_DEP_SRC="${CONV_VALUE_DEP_SRC:-$EXAMPLE_DIR/conv2d_kernel_scair_value_dependent.mlir}"
VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver_bare.c}"

OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
COMPILER_FLAGS="-O2/-O3"

MLIR_PIPELINE="--lower-affine --convert-scf-to-cf --expand-strided-metadata --finalize-memref-to-llvm --convert-arith-to-llvm --convert-index-to-llvm --convert-cf-to-llvm --convert-func-to-llvm --reconcile-unrealized-casts"
SCAIR_BASELINE_PIPELINE="lower-dynamic-memref-to-llvm-baseline,convert-func-to-llvm,convert-llvm-export-abi"
SCAIR_VALUE_DEP_PIPELINE="lower-dmemref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$COMPILE_TIME_BENCH"
require_file "$CONV_MLIR_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$CONV_BASELINE_SRC"
require_file "$BASELINE_DRIVER_SRC"
require_file "$CONV_VALUE_DEP_SRC"
require_file "$VALUE_DEP_DRIVER_SRC"

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

build_scair_kernel() {
  local route="$1"
  local src="$2"
  local obj_out="$3"
  local llvm_ir_out="$4"
  local lowered_mlir_out="${llvm_ir_out%.ll}.llvm.mlir"

  "$SCAIR_OPT" -s "$src" --passes "$route" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' \
    > "$lowered_mlir_out"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_out" > "$llvm_ir_out"
  "$CC" -O3 -x ir "$llvm_ir_out" -c -o "$obj_out"
}

build_mlir_kernel() {
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
  "$CC" -O3 -x ir "$llvm_ir_out" -c -o "$obj_out"
}

run_with_metrics() {
  local exe="$1"
  local output_txt="$2"
  local n="$3"
  local cin="$4"
  local h="$5"
  local w="$6"
  local cout="$7"
  local kh="$8"
  local kw="$9"
  local iterations="${10:-$ITERATIONS}"

  set +e
  run_benchmark_repeated "$output_txt" "$exe" "$n" "$cin" "$h" "$w" "$cout" "$kh" "$kw" "$iterations"
  local status=$?
  set -e
  if [[ $status -ne 0 ]]; then
    echo "error: Conv2D benchmark run failed for size n=$n cin=$cin h=$h w=$w cout=$cout kh=$kh kw=$kw using $exe" >&2
    exit $status
  fi
}

iterations_for_size() {
  local n="$1"
  local h="$2"
  local w="$3"
  local cout="$4"
  local oh="$5"
  local ow="$6"
  local output_elements=$((n * cout * oh * ow))

  if [[ "$output_elements" -ge "$CONV_LARGE_OUTPUT_ELEMENTS_THRESHOLD" ]]; then
    echo "$CONV_LARGE_ITERATIONS"
  else
    echo "$ITERATIONS"
  fi
}

append_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local variant="$3"
  local representation="$4"
  local src="$5"
  local lowered="$6"
  local llvm="$7"
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
    "convolution_benchmark" \
    "conv2d_kernel" \
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
    "$(count_func_defs "$lowered")" \
    "$(count_ops "$lowered")" \
    "$(count_ops_structural "$lowered")" \
    "$(file_metric lines "$lowered")" \
    "$(file_metric lines "$llvm")" \
    "$(count_llvm_calls "$llvm")" \
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
    "conv2d" \
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
    "conv2d_kernel" \
    "$variant" \
    "$size_descriptor" \
    "ok" \
    "$(metric_field run_status "$output_txt")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$lowered")" \
    "$(file_metric lines "$llvm")" \
    "$(json_top_level_field "$timing_json" total_ms)" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

echo "==> Building upstream MLIR baseline kernel"
MLIR_TIMING_JSON="$OUT_DIR/conv2d_mlir_baseline.compile_timing.json"
measure_compile_timing "$MLIR_OPT" "$CONV_MLIR_SRC" "$MLIR_PIPELINE" "$MLIR_TIMING_JSON"
mlir_start=$(now_ns)
build_mlir_kernel "$CONV_MLIR_SRC" "$OUT_DIR/conv2d_mlir_baseline.o" "$OUT_DIR/conv2d_mlir_baseline.llvm.mlir" "$OUT_DIR/conv2d_mlir_baseline.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"conv2d_kernel\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$MLIR_DRIVER_SRC" \
  "$OUT_DIR/conv2d_mlir_baseline.o" \
  -o "$OUT_DIR/conv2d_mlir_baseline_exec"
mlir_end=$(now_ns)
MLIR_COMPILE_MS="$(format_ms "$mlir_start" "$mlir_end")"

echo "==> Building ScaIR baseline kernel"
SCAIR_BASELINE_TIMING_JSON="$OUT_DIR/conv2d_scair_baseline.compile_timing.json"
measure_compile_timing "$SCAIR_OPT" "$CONV_BASELINE_SRC" "$SCAIR_BASELINE_PIPELINE" "$SCAIR_BASELINE_TIMING_JSON"
baseline_start=$(now_ns)
build_scair_kernel "$SCAIR_BASELINE_PIPELINE" "$CONV_BASELINE_SRC" "$OUT_DIR/conv2d_baseline_kernel_only_scair.o" "$OUT_DIR/conv2d_baseline_kernel_only_scair.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"conv2d_kernel\"" \
  -DVARIANT_LABEL="\"scair_baseline\"" \
  "$BASELINE_DRIVER_SRC" \
  "$OUT_DIR/conv2d_baseline_kernel_only_scair.o" \
  -o "$OUT_DIR/conv2d_baseline_kernel_only_scair_exec"
baseline_end=$(now_ns)
BASELINE_COMPILE_MS="$(format_ms "$baseline_start" "$baseline_end")"

echo "==> Building ScaIR value-dependent kernel"
SCAIR_VALUE_DEP_TIMING_JSON="$OUT_DIR/conv2d_value_dependent.compile_timing.json"
measure_compile_timing "$SCAIR_OPT" "$CONV_VALUE_DEP_SRC" "$SCAIR_VALUE_DEP_PIPELINE" "$SCAIR_VALUE_DEP_TIMING_JSON"
value_start=$(now_ns)
build_scair_kernel "$SCAIR_VALUE_DEP_PIPELINE" "$CONV_VALUE_DEP_SRC" "$OUT_DIR/conv2d_value_dependent_scair.o" "$OUT_DIR/conv2d_value_dependent_scair.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"conv2d_kernel\"" \
  -DVARIANT_LABEL="\"value_dependent\"" \
  "$VALUE_DEP_DRIVER_SRC" \
  "$OUT_DIR/conv2d_value_dependent_scair.o" \
  -o "$OUT_DIR/conv2d_value_dependent_scair_exec"
value_end=$(now_ns)
VALUE_COMPILE_MS="$(format_ms "$value_start" "$value_end")"

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Convolution Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

IFS=',' read -r -a CONV_SIZES <<<"$CONV_SIZE_SET"
for dims in "${CONV_SIZES[@]}"; do
  IFS='x' read -r n cin h w cout kh kw <<<"$dims"
  oh=$((h - kh + 1))
  ow=$((w - kw + 1))
  if [[ "$oh" -le 0 || "$ow" -le 0 ]]; then
    echo "error: invalid convolution dimensions: $dims" >&2
    exit 1
  fi

  size_descriptor="n=${n};cin=${cin};h=${h};w=${w};cout=${cout};kh=${kh};kw=${kw};layout=NCHW/OIHW"
  tag="$(size_tag "$dims")"
  size_iterations="$(iterations_for_size "$n" "$h" "$w" "$cout" "$oh" "$ow")"

  run_with_metrics "$OUT_DIR/conv2d_mlir_baseline_exec" "$OUT_DIR/conv2d_mlir_baseline_${tag}_output.txt" "$n" "$cin" "$h" "$w" "$cout" "$kh" "$kw" "$size_iterations"
  append_row \
    "$SUMMARY_CSV" "$SUMMARY_MD" \
    "mlir_baseline" "mlir_baseline" \
    "$CONV_MLIR_SRC" \
    "$OUT_DIR/conv2d_mlir_baseline.llvm.mlir" \
    "$OUT_DIR/conv2d_mlir_baseline.ll" \
    "$OUT_DIR/conv2d_mlir_baseline_${tag}_output.txt" \
    "$MLIR_COMPILE_MS" \
    "$MLIR_TIMING_JSON" \
    "$size_descriptor" \
    "upstream MLIR fixed lowering pipeline; timed region includes output reset plus kernel execution; host-side checksum excluded;driver_iterations=$size_iterations"

  run_with_metrics "$OUT_DIR/conv2d_baseline_kernel_only_scair_exec" "$OUT_DIR/conv2d_baseline_kernel_only_scair_${tag}_output.txt" "$n" "$cin" "$h" "$w" "$cout" "$kh" "$kw" "$size_iterations"
  append_row \
    "$SUMMARY_CSV" "$SUMMARY_MD" \
    "scair_baseline" "scair_baseline" \
    "$CONV_BASELINE_SRC" \
    "$OUT_DIR/conv2d_baseline_kernel_only_scair.llvm.mlir" \
    "$OUT_DIR/conv2d_baseline_kernel_only_scair.ll" \
    "$OUT_DIR/conv2d_baseline_kernel_only_scair_${tag}_output.txt" \
    "$BASELINE_COMPILE_MS" \
    "$SCAIR_BASELINE_TIMING_JSON" \
    "$size_descriptor" \
    "ScaIR dynamic baseline kernel route; timed region includes output reset plus kernel execution; host-side checksum excluded;driver_iterations=$size_iterations"

  run_with_metrics "$OUT_DIR/conv2d_value_dependent_scair_exec" "$OUT_DIR/conv2d_value_dependent_scair_${tag}_output.txt" "$n" "$cin" "$h" "$w" "$cout" "$kh" "$kw" "$size_iterations"
  append_row \
    "$SUMMARY_CSV" "$SUMMARY_MD" \
    "value_dependent" "value_dependent" \
    "$CONV_VALUE_DEP_SRC" \
    "$OUT_DIR/conv2d_value_dependent_scair.llvm.mlir" \
    "$OUT_DIR/conv2d_value_dependent_scair.ll" \
    "$OUT_DIR/conv2d_value_dependent_scair_${tag}_output.txt" \
    "$VALUE_COMPILE_MS" \
    "$SCAIR_VALUE_DEP_TIMING_JSON" \
    "$size_descriptor" \
    "ScaIR value-dependent executable; timed region includes output reset plus kernel execution; host-side checksum excluded;driver_iterations=$size_iterations"
done

echo
echo "Convolution build complete."
echo "Produced:"
echo "  $OUT_DIR/conv2d_mlir_baseline_exec"
echo "  $OUT_DIR/conv2d_baseline_kernel_only_scair_exec"
echo "  $OUT_DIR/conv2d_value_dependent_scair_exec"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
