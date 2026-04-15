#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-100}"
GEMM_N="${GEMM_N:-32}"
GEMM_M="${GEMM_M:-32}"
GEMM_K="${GEMM_K:-32}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="$BIN_DIR/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"

MLIR_SRC="${MLIR_SRC:-$EXAMPLE_DIR/matmul_kernel_mlir_baseline.mlir}"
SCAIR_SRC="${SCAIR_SRC:-$EXAMPLE_DIR/matmul_kernel_scair_dmemref.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver_mlir.c}"
SCAIR_DRIVER_SRC="${SCAIR_DRIVER_SRC:-$EXAMPLE_DIR/driver_scair.c}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/build_scair}"

mkdir -p "$OUT_DIR"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$MLIR_SRC"
require_file "$SCAIR_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$SCAIR_DRIVER_SRC"

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
  "$CC" -O2 -x ir "$llvm_ir_out" -c -o "$obj_out"
}

build_scair_kernel() {
  local src="$1"
  local obj_out="$2"
  local lowered_mlir_out="$3"
  local llvm_ir_out="$4"

  "$SCAIR_OPT" "$src" --passes "lower-dmemref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' \
    > "$lowered_mlir_out"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_out" > "$llvm_ir_out"
  "$CC" -O2 -x ir "$llvm_ir_out" -c -o "$obj_out"
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
  local notes="${10}"

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
    "NA"

  append_summary_row \
    "$summary_md" \
    "matmul_strided" \
    "$variant" \
    "" \
    "ok" \
    "$(metric_field run_status "$output_txt")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$lowered_mlir")" \
    "$(file_metric lines "$llvm_ir")" \
    "$compile_ms" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

echo "==> Building upstream MLIR strided matmul"
mlir_start=$(now_ns)
build_mlir_kernel "$MLIR_SRC" "$OUT_DIR/matmul_mlir_baseline.o" "$OUT_DIR/matmul_mlir_baseline.llvm.mlir" "$OUT_DIR/matmul_mlir_baseline.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"strided_matmul\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$MLIR_DRIVER_SRC" \
  "$OUT_DIR/matmul_mlir_baseline.o" \
  -o "$OUT_DIR/matmul_mlir_baseline_exec"
mlir_end=$(now_ns)
run_benchmark_repeated "$OUT_DIR/matmul_mlir_baseline_output.txt" \
  "$OUT_DIR/matmul_mlir_baseline_exec" "$GEMM_N" "$GEMM_M" "$GEMM_K" "$ITERATIONS"
MLIR_COMPILE_MS="$(format_ms "$mlir_start" "$mlir_end")"

echo "==> Building SCAIR d_memref strided matmul"
scair_start=$(now_ns)
build_scair_kernel "$SCAIR_SRC" "$OUT_DIR/matmul_scair_dmemref.o" "$OUT_DIR/matmul_scair_dmemref.llvm.mlir" "$OUT_DIR/matmul_scair_dmemref.ll"
"$CC" -O2 \
  -DBENCH_LABEL="\"strided_matmul\"" \
  -DVARIANT_LABEL="\"scair_dmemref\"" \
  "$SCAIR_DRIVER_SRC" \
  "$OUT_DIR/matmul_scair_dmemref.o" \
  -o "$OUT_DIR/matmul_scair_dmemref_exec"
scair_end=$(now_ns)
run_benchmark_repeated "$OUT_DIR/matmul_scair_dmemref_output.txt" \
  "$OUT_DIR/matmul_scair_dmemref_exec" "$GEMM_N" "$GEMM_M" "$GEMM_K" "$ITERATIONS"
SCAIR_COMPILE_MS="$(format_ms "$scair_start" "$scair_end")"

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Strided Matmul Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

append_row \
  "$SUMMARY_CSV" "$SUMMARY_MD" \
  "mlir_baseline" "mlir_baseline" \
  "$MLIR_SRC" \
  "$OUT_DIR/matmul_mlir_baseline.llvm.mlir" \
  "$OUT_DIR/matmul_mlir_baseline.ll" \
  "$OUT_DIR/matmul_mlir_baseline_output.txt" \
  "$MLIR_COMPILE_MS" \
  "flat memrefs plus memref.reinterpret_cast strided matrix views"

append_row \
  "$SUMMARY_CSV" "$SUMMARY_MD" \
  "scair_dmemref" "value_dependent" \
  "$SCAIR_SRC" \
  "$OUT_DIR/matmul_scair_dmemref.llvm.mlir" \
  "$OUT_DIR/matmul_scair_dmemref.ll" \
  "$OUT_DIR/matmul_scair_dmemref_output.txt" \
  "$SCAIR_COMPILE_MS" \
  "flat d_memref buffers plus d_memref.reinterpret_cast refined matrix views"

echo
echo "Strided matmul build complete."
echo "Produced:"
echo "  $OUT_DIR/matmul_mlir_baseline_exec"
echo "  $OUT_DIR/matmul_scair_dmemref_exec"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
