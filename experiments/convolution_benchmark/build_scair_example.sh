#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-200}"
CONV_N="${CONV_N:-1}"
CONV_CIN="${CONV_CIN:-8}"
CONV_H="${CONV_H:-32}"
CONV_W="${CONV_W:-32}"
CONV_COUT="${CONV_COUT:-16}"
CONV_KH="${CONV_KH:-3}"
CONV_KW="${CONV_KW:-3}"

CONV_OH=$((CONV_H - CONV_KH + 1))
CONV_OW=$((CONV_W - CONV_KW + 1))
if [[ "$CONV_OH" -le 0 || "$CONV_OW" -le 0 ]]; then
  echo "error: invalid convolution dimensions" >&2
  exit 1
fi

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="${BIN_DIR}/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"

CONV_MLIR_SRC="${CONV_MLIR_SRC:-$EXAMPLE_DIR/conv2d_kernel.mlir}"
CHECKSUM_MLIR_SRC="${CHECKSUM_MLIR_SRC:-$EXAMPLE_DIR/checksum_kernel.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"

CONV_BASELINE_SRC="${CONV_BASELINE_SRC:-$EXAMPLE_DIR/conv2d_kernel_scair_baseline.mlir}"
CHECKSUM_BASELINE_SRC="${CHECKSUM_BASELINE_SRC:-$EXAMPLE_DIR/checksum_kernel_scair_baseline.mlir}"
BASELINE_DRIVER_SRC="${BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline_bare.c}"

CONV_VALUE_DEP_SRC="${CONV_VALUE_DEP_SRC:-$EXAMPLE_DIR/conv2d_kernel_scair_value_dependent.mlir}"
CHECKSUM_VALUE_DEP_SRC="${CHECKSUM_VALUE_DEP_SRC:-$EXAMPLE_DIR/checksum_kernel_scair_value_dependent.mlir}"
VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver_bare.c}"

OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
mkdir -p "$OUT_DIR"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"

require_file "$CONV_MLIR_SRC"
require_file "$CHECKSUM_MLIR_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$CONV_BASELINE_SRC"
require_file "$CHECKSUM_BASELINE_SRC"
require_file "$BASELINE_DRIVER_SRC"
require_file "$CONV_VALUE_DEP_SRC"
require_file "$CHECKSUM_VALUE_DEP_SRC"
require_file "$VALUE_DEP_DRIVER_SRC"

build_scair_kernel() {
  local route="$1"
  local src="$2"
  local obj_out="$3"
  local llvm_ir_out="$4"
  local lowered_mlir_out="${llvm_ir_out%.ll}.llvm.mlir"

  "$SCAIR_OPT" -s "$src" --passes "$route,convert-func-to-llvm,convert-llvm-export-abi" \
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
  run_benchmark_repeated "$output_txt" \
    "$exe" "$CONV_N" "$CONV_CIN" "$CONV_H" "$CONV_W" "$CONV_COUT" "$CONV_KH" "$CONV_KW" "$ITERATIONS"
}

sum_two() {
  sum_numeric_or_na "$1" "$2"
}

append_pair_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local variant="$3"
  local representation="$4"
  local src_a="$5"
  local src_b="$6"
  local lowered_a="$7"
  local lowered_b="$8"
  local llvm_a="$9"
  local llvm_b="${10}"
  local output_txt="${11}"
  local compile_ms="${12}"
  local notes="${13}"

  append_metrics_csv_row \
    "$metrics_csv" \
    "convolution_benchmark" \
    "conv2d_checksum" \
    "$variant" \
    "$representation" \
    "ok" \
    "$(metric_field run_status "$output_txt")" \
    "$(sum_two "$(file_metric bytes "$src_a")" "$(file_metric bytes "$src_b")")" \
    "$(sum_two "$(file_metric lines "$src_a")" "$(file_metric lines "$src_b")")" \
    "$(sum_two "$(count_ops "$src_a")" "$(count_ops "$src_b")")" \
    "$(sum_two "$(count_ops_structural "$src_a")" "$(count_ops_structural "$src_b")")" \
    "$(sum_two "$(count_func_defs "$src_a")" "$(count_func_defs "$src_b")")" \
    "$(sum_two "$(count_block_args "$src_a")" "$(count_block_args "$src_b")")" \
    "$(sum_two "$(count_alloc_ops "$src_a")" "$(count_alloc_ops "$src_b")")" \
    "$(sum_two "$(count_source_reinterpret_cast_ops "$src_a")" "$(count_source_reinterpret_cast_ops "$src_b")")" \
    "$(sum_two "$(count_source_subview_ops "$src_a")" "$(count_source_subview_ops "$src_b")")" \
    "$(sum_two "$(count_source_extract_strided_metadata_ops "$src_a")" "$(count_source_extract_strided_metadata_ops "$src_b")")" \
    "$(sum_two "$(count_source_memref_load_ops "$src_a")" "$(count_source_memref_load_ops "$src_b")")" \
    "$(sum_two "$(count_source_memref_store_ops "$src_a")" "$(count_source_memref_store_ops "$src_b")")" \
    "$(sum_two "$(count_source_dmemref_load_ops "$src_a")" "$(count_source_dmemref_load_ops "$src_b")")" \
    "$(sum_two "$(count_source_dmemref_store_ops "$src_a")" "$(count_source_dmemref_store_ops "$src_b")")" \
    "$(sum_two "$(count_func_defs "$lowered_a")" "$(count_func_defs "$lowered_b")")" \
    "$(sum_two "$(count_ops "$lowered_a")" "$(count_ops "$lowered_b")")" \
    "$(sum_two "$(count_ops_structural "$lowered_a")" "$(count_ops_structural "$lowered_b")")" \
    "$(sum_two "$(file_metric lines "$lowered_a")" "$(file_metric lines "$lowered_b")")" \
    "$(sum_two "$(file_metric lines "$llvm_a")" "$(file_metric lines "$llvm_b")")" \
    "$(sum_two "$(count_llvm_calls "$llvm_a")" "$(count_llvm_calls "$llvm_b")")" \
    "$compile_ms" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "$notes" \
    "$(sum_two "$(count_source_helpers "$src_a")" "$(count_source_helpers "$src_b")")" \
    "0" \
    "0" \
    "NA" \
    "NA"

  append_summary_row \
    "$summary_md" \
    "conv2d_checksum" \
    "$variant" \
    "" \
    "ok" \
    "$(metric_field run_status "$output_txt")" \
    "$(sum_two "$(count_ops_structural "$src_a")" "$(count_ops_structural "$src_b")")" \
    "$(sum_two "$(count_func_defs "$src_a")" "$(count_func_defs "$src_b")")" \
    "$(sum_two "$(count_block_args "$src_a")" "$(count_block_args "$src_b")")" \
    "$(sum_two "$(file_metric lines "$lowered_a")" "$(file_metric lines "$lowered_b")")" \
    "$(sum_two "$(file_metric lines "$llvm_a")" "$(file_metric lines "$llvm_b")")" \
    "$compile_ms" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

echo "==> Building upstream MLIR baseline kernels"
mlir_baseline_start=$(now_ns)
build_mlir_kernel "$CONV_MLIR_SRC" "$OUT_DIR/conv2d_mlir_baseline.o" "$OUT_DIR/conv2d_mlir_baseline.llvm.mlir" "$OUT_DIR/conv2d_mlir_baseline.ll"
build_mlir_kernel "$CHECKSUM_MLIR_SRC" "$OUT_DIR/checksum_mlir_baseline.o" "$OUT_DIR/checksum_mlir_baseline.llvm.mlir" "$OUT_DIR/checksum_mlir_baseline.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"conv2d_checksum\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$MLIR_DRIVER_SRC" \
  "$OUT_DIR/conv2d_mlir_baseline.o" \
  "$OUT_DIR/checksum_mlir_baseline.o" \
  -o "$OUT_DIR/conv2d_mlir_baseline_exec"
mlir_baseline_end=$(now_ns)
run_with_metrics "$OUT_DIR/conv2d_mlir_baseline_exec" "$OUT_DIR/conv2d_mlir_baseline_output.txt"
MLIR_BASELINE_COMPILE_MS="$(format_ms "$mlir_baseline_start" "$mlir_baseline_end")"

echo "==> Building ScaIR baseline kernel-only split kernels"
baseline_start=$(now_ns)
build_scair_kernel "lower-dynamic-memref-to-llvm-baseline" "$CONV_BASELINE_SRC" "$OUT_DIR/conv2d_baseline_kernel_only_scair.o" "$OUT_DIR/conv2d_baseline_kernel_only_scair.ll"
build_scair_kernel "lower-dynamic-memref-to-llvm-baseline" "$CHECKSUM_BASELINE_SRC" "$OUT_DIR/checksum_baseline_kernel_only_scair.o" "$OUT_DIR/checksum_baseline_kernel_only_scair.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"conv2d_checksum\"" \
  -DVARIANT_LABEL="\"scair_baseline\"" \
  "$BASELINE_DRIVER_SRC" \
  "$OUT_DIR/conv2d_baseline_kernel_only_scair.o" \
  "$OUT_DIR/checksum_baseline_kernel_only_scair.o" \
  -o "$OUT_DIR/conv2d_baseline_kernel_only_scair_exec"
baseline_end=$(now_ns)
run_with_metrics "$OUT_DIR/conv2d_baseline_kernel_only_scair_exec" "$OUT_DIR/conv2d_baseline_kernel_only_scair_output.txt"
BASELINE_COMPILE_MS="$(format_ms "$baseline_start" "$baseline_end")"

echo "==> Building ScaIR value-dependent split kernels"
value_start=$(now_ns)
build_scair_kernel "lower-dmemref-to-llvm" "$CONV_VALUE_DEP_SRC" "$OUT_DIR/conv2d_value_dependent_scair.o" "$OUT_DIR/conv2d_value_dependent_scair.ll"
build_scair_kernel "lower-dmemref-to-llvm" "$CHECKSUM_VALUE_DEP_SRC" "$OUT_DIR/checksum_value_dependent_scair.o" "$OUT_DIR/checksum_value_dependent_scair.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"conv2d_checksum\"" \
  -DVARIANT_LABEL="\"value_dependent\"" \
  "$VALUE_DEP_DRIVER_SRC" \
  "$OUT_DIR/conv2d_value_dependent_scair.o" \
  "$OUT_DIR/checksum_value_dependent_scair.o" \
  -o "$OUT_DIR/conv2d_value_dependent_scair_exec"
value_end=$(now_ns)
run_with_metrics "$OUT_DIR/conv2d_value_dependent_scair_exec" "$OUT_DIR/conv2d_value_dependent_scair_output.txt"
VALUE_COMPILE_MS="$(format_ms "$value_start" "$value_end")"

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Convolution Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

append_pair_row \
  "$SUMMARY_CSV" "$SUMMARY_MD" \
  "mlir_baseline" "mlir_baseline" \
  "$CONV_MLIR_SRC" "$CHECKSUM_MLIR_SRC" \
  "$OUT_DIR/conv2d_mlir_baseline.llvm.mlir" "$OUT_DIR/checksum_mlir_baseline.llvm.mlir" \
  "$OUT_DIR/conv2d_mlir_baseline.ll" "$OUT_DIR/checksum_mlir_baseline.ll" \
  "$OUT_DIR/conv2d_mlir_baseline_output.txt" \
  "$MLIR_BASELINE_COMPILE_MS" \
  "upstream MLIR fixed lowering pipeline"

append_pair_row \
  "$SUMMARY_CSV" "$SUMMARY_MD" \
  "scair_baseline" "scair_baseline" \
  "$CONV_BASELINE_SRC" "$CHECKSUM_BASELINE_SRC" \
  "$OUT_DIR/conv2d_baseline_kernel_only_scair.llvm.mlir" "$OUT_DIR/checksum_baseline_kernel_only_scair.llvm.mlir" \
  "$OUT_DIR/conv2d_baseline_kernel_only_scair.ll" "$OUT_DIR/checksum_baseline_kernel_only_scair.ll" \
  "$OUT_DIR/conv2d_baseline_kernel_only_scair_output.txt" \
  "$BASELINE_COMPILE_MS" \
  "ScaIR baseline kernel-only executable"

append_pair_row \
  "$SUMMARY_CSV" "$SUMMARY_MD" \
  "value_dependent" "value_dependent" \
  "$CONV_VALUE_DEP_SRC" "$CHECKSUM_VALUE_DEP_SRC" \
  "$OUT_DIR/conv2d_value_dependent_scair.llvm.mlir" "$OUT_DIR/checksum_value_dependent_scair.llvm.mlir" \
  "$OUT_DIR/conv2d_value_dependent_scair.ll" "$OUT_DIR/checksum_value_dependent_scair.ll" \
  "$OUT_DIR/conv2d_value_dependent_scair_output.txt" \
  "$VALUE_COMPILE_MS" \
  "ScaIR value-dependent executable"

echo
echo "ScaIR build complete."
echo "Produced:"
echo "  $OUT_DIR/conv2d_mlir_baseline_exec"
echo "  $OUT_DIR/conv2d_baseline_kernel_only_scair_exec"
echo "  $OUT_DIR/conv2d_value_dependent_scair_exec"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
