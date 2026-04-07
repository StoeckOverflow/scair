#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-10000}"
RUNTIME_N="${RUNTIME_N:-16}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
MLIR_OPT="$BIN_DIR/mlir-opt"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/build_scair}"
ALLOC_DRIVER_SRC="${ALLOC_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"

SUBVIEW_MLIR_BASELINE_SRC="${SUBVIEW_MLIR_BASELINE_SRC:-$EXAMPLE_DIR/control_flow_selected_subview_reduction_mlir_baseline.mlir}"
ALLOC_MLIR_BASELINE_SRC="${ALLOC_MLIR_BASELINE_SRC:-$EXAMPLE_DIR/control_flow_selected_allocation_reduction_mlir_baseline.mlir}"

SUBVIEW_BASELINE_SRC="${SUBVIEW_BASELINE_SRC:-$EXAMPLE_DIR/control_flow_selected_subview_reduction_baseline.mlir}"
SUBVIEW_VALUE_DEP_SRC="${SUBVIEW_VALUE_DEP_SRC:-$EXAMPLE_DIR/control_flow_selected_subview_reduction_value_dependent.mlir}"
SUBVIEW_BASELINE_DRIVER_SRC="${SUBVIEW_BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline_bare.c}"
SUBVIEW_VALUE_DEP_DRIVER_SRC="${SUBVIEW_VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver_bare.c}"

ALLOC_BASELINE_SRC="${ALLOC_BASELINE_SRC:-$EXAMPLE_DIR/control_flow_selected_allocation_reduction_baseline.mlir}"
ALLOC_VALUE_DEP_SRC="${ALLOC_VALUE_DEP_SRC:-$EXAMPLE_DIR/control_flow_selected_allocation_reduction_value_dependent.mlir}"

mkdir -p "$OUT_DIR"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"

require_file "$SUBVIEW_MLIR_BASELINE_SRC"
require_file "$ALLOC_MLIR_BASELINE_SRC"
require_file "$SUBVIEW_BASELINE_SRC"
require_file "$SUBVIEW_VALUE_DEP_SRC"
require_file "$SUBVIEW_BASELINE_DRIVER_SRC"
require_file "$SUBVIEW_VALUE_DEP_DRIVER_SRC"
require_file "$ALLOC_BASELINE_SRC"
require_file "$ALLOC_VALUE_DEP_SRC"
require_file "$ALLOC_DRIVER_SRC"

build_kernel() {
  local route="$1"
  local src="$2"
  local obj_out="$3"
  local llvm_ir_out="$4"
  local metrics_out="$5"
  local lowered_mlir_out="${llvm_ir_out%.ll}.llvm.mlir"

  local start_ns
  local end_ns
  start_ns=$(now_ns)

  "$SCAIR_OPT" "$src" --passes "$route,convert-func-to-llvm,convert-llvm-export-abi" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' \
    > "$lowered_mlir_out"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_out" > "$llvm_ir_out"
  "$CC" -O2 -x ir "$llvm_ir_out" -c -o "$obj_out"

  end_ns=$(now_ns)
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } > "$metrics_out"
}

build_mlir_kernel() {
  local src="$1"
  local obj_out="$2"
  local llvm_ir_out="$3"
  local metrics_out="$4"
  local lowered_mlir_out="${llvm_ir_out%.ll}.llvm.mlir"

  local start_ns
  local end_ns
  start_ns=$(now_ns)

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

  end_ns=$(now_ns)
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } > "$metrics_out"
}

append_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local benchmark="$3"
  local variant="$4"
  local representation="$5"
  local src="$6"
  local lowered_mlir="$7"
  local llvm_ir="$8"
  local output_txt="$9"
  local notes="${10}"

  append_metrics_csv_row \
    "$metrics_csv" \
    "memref_control_flow" \
    "$benchmark" \
    "$variant" \
    "$representation" \
    "$(metric_field build_status "$output_txt")" \
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
    "$(file_metric lines "$llvm_ir")" \
    "$(count_llvm_calls "$llvm_ir")" \
    "$(metric_field compile_ms "$output_txt")" \
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
    "$benchmark" \
    "$variant" \
    "$notes" \
    "$(metric_field build_status "$output_txt")" \
    "$(metric_field run_status "$output_txt")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

echo "==> Building upstream MLIR memref-control-flow control_flow_selected_subview_reduction baseline"
build_mlir_kernel \
  "$SUBVIEW_MLIR_BASELINE_SRC" \
  "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.o" \
  "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.ll" \
  "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_metrics.txt"

echo "==> Linking upstream MLIR memref-control-flow control_flow_selected_subview_reduction baseline executable"
"$CC" -O2 \
  -DMLIR_C_INTERFACE \
  -DBENCH_LABEL="\"control_flow_selected_subview_reduction\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$SUBVIEW_BASELINE_DRIVER_SRC" \
  "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.o" \
  -o "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_exec"

echo "==> Building ScaIR memref-control-flow control_flow_selected_subview_reduction baseline kernel-only"
build_kernel \
  "lower-dynamic-memref-to-llvm-baseline" \
  "$SUBVIEW_BASELINE_SRC" \
  "$OUT_DIR/control_flow_selected_subview_reduction_baseline.o" \
  "$OUT_DIR/control_flow_selected_subview_reduction_baseline.ll" \
  "$OUT_DIR/control_flow_selected_subview_reduction_baseline_metrics.txt"

echo "==> Linking ScaIR memref-control-flow control_flow_selected_subview_reduction baseline executable"
"$CC" -O2 \
  -DBENCH_LABEL="\"control_flow_selected_subview_reduction\"" \
  -DVARIANT_LABEL="\"baseline\"" \
  "$SUBVIEW_BASELINE_DRIVER_SRC" \
  "$OUT_DIR/control_flow_selected_subview_reduction_baseline.o" \
  -o "$OUT_DIR/control_flow_selected_subview_reduction_baseline_exec"

echo "==> Building ScaIR memref-control-flow control_flow_selected_subview_reduction value-dependent kernel-only"
build_kernel \
  "lower-dmemref-to-llvm" \
  "$SUBVIEW_VALUE_DEP_SRC" \
  "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.o" \
  "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.ll" \
  "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_metrics.txt"

echo "==> Linking ScaIR memref-control-flow control_flow_selected_subview_reduction value-dependent executable"
"$CC" -O2 \
  -DBENCH_LABEL="\"control_flow_selected_subview_reduction\"" \
  -DVARIANT_LABEL="\"value_dependent\"" \
  "$SUBVIEW_VALUE_DEP_DRIVER_SRC" \
  "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.o" \
  -o "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_exec"

echo "==> Building ScaIR memref-control-flow control_flow_selected_allocation_reduction baseline kernel-only"
build_kernel \
  "lower-dynamic-memref-to-llvm-baseline" \
  "$ALLOC_BASELINE_SRC" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.o" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.ll" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_metrics.txt"

echo "==> Linking ScaIR memref-control-flow control_flow_selected_allocation_reduction baseline executable"
"$CC" -O2 \
  -DBASELINE_MEMREF_ABI \
  -DBENCH_LABEL="\"control_flow_selected_allocation_reduction\"" \
  -DVARIANT_LABEL="\"baseline\"" \
  "$ALLOC_DRIVER_SRC" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.o" \
  -o "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_exec"

echo "==> Building ScaIR memref-control-flow control_flow_selected_allocation_reduction value-dependent kernel-only"
build_kernel \
  "lower-dmemref-to-llvm" \
  "$ALLOC_VALUE_DEP_SRC" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.o" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.ll" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_metrics.txt"

echo "==> Linking ScaIR memref-control-flow control_flow_selected_allocation_reduction value-dependent executable"
"$CC" -O2 \
  -DBENCH_LABEL="\"control_flow_selected_allocation_reduction\"" \
  -DVARIANT_LABEL="\"value_dependent\"" \
  "$ALLOC_DRIVER_SRC" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.o" \
  -o "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_exec"

echo "==> Building upstream MLIR memref-control-flow control_flow_selected_allocation_reduction baseline"
build_mlir_kernel \
  "$ALLOC_MLIR_BASELINE_SRC" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.o" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.ll" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_metrics.txt"

echo "==> Linking upstream MLIR memref-control-flow control_flow_selected_allocation_reduction baseline executable"
"$CC" -O2 \
  -DMLIR_C_INTERFACE \
  -DBENCH_LABEL="\"control_flow_selected_allocation_reduction\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$ALLOC_DRIVER_SRC" \
  "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.o" \
  -o "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_exec"

run_and_merge() {
  local exe="$1"
  local output_txt="$2"
  shift 2
  run_benchmark_repeated "$output_txt" "$exe" "$@"
}

run_and_merge "$OUT_DIR/control_flow_selected_subview_reduction_baseline_exec" "$OUT_DIR/control_flow_selected_subview_reduction_baseline_selector0_output.txt" 0 "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_subview_reduction_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_subview_reduction_baseline_selector0_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_exec" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_selector0_output.txt" 0 "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_selector0_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_subview_reduction_baseline_exec" "$OUT_DIR/control_flow_selected_subview_reduction_baseline_selector1_output.txt" 1 "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_subview_reduction_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_subview_reduction_baseline_selector1_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_exec" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_selector1_output.txt" 1 "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_selector1_output.txt"

run_and_merge "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_exec" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_selector0_output.txt" 0 "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_metrics.txt" >> "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_selector0_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_exec" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_selector1_output.txt" 1 "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_metrics.txt" >> "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_selector1_output.txt"

run_and_merge "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_exec" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_selector0_output.txt" 0 "$RUNTIME_N" "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_selector0_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_exec" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_selector0_output.txt" 0 "$RUNTIME_N" "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_selector0_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_exec" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_selector1_output.txt" 1 "$RUNTIME_N" "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_selector1_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_exec" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_selector1_output.txt" 1 "$RUNTIME_N" "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_metrics.txt" >> "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_selector1_output.txt"

run_and_merge "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_exec" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_selector0_output.txt" 0 "$RUNTIME_N" "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_metrics.txt" >> "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_selector0_output.txt"
run_and_merge "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_exec" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_selector1_output.txt" 1 "$RUNTIME_N" "$ITERATIONS"
cat "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_metrics.txt" >> "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_selector1_output.txt"

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Memref Control-Flow Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_subview_reduction" "mlir_baseline" "mlir_baseline" "$SUBVIEW_MLIR_BASELINE_SRC" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.ll" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_selector0_output.txt" "selector=0"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_subview_reduction" "mlir_baseline" "mlir_baseline" "$SUBVIEW_MLIR_BASELINE_SRC" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline.ll" "$OUT_DIR/control_flow_selected_subview_reduction_mlir_baseline_selector1_output.txt" "selector=1"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_subview_reduction" "scair_baseline" "scair_baseline" "$SUBVIEW_BASELINE_SRC" "$OUT_DIR/control_flow_selected_subview_reduction_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_subview_reduction_baseline.ll" "$OUT_DIR/control_flow_selected_subview_reduction_baseline_selector0_output.txt" "selector=0"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_subview_reduction" "scair_baseline" "scair_baseline" "$SUBVIEW_BASELINE_SRC" "$OUT_DIR/control_flow_selected_subview_reduction_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_subview_reduction_baseline.ll" "$OUT_DIR/control_flow_selected_subview_reduction_baseline_selector1_output.txt" "selector=1"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_subview_reduction" "value_dependent" "value_dependent" "$SUBVIEW_VALUE_DEP_SRC" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.llvm.mlir" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.ll" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_selector0_output.txt" "selector=0"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_subview_reduction" "value_dependent" "value_dependent" "$SUBVIEW_VALUE_DEP_SRC" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.llvm.mlir" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent.ll" "$OUT_DIR/control_flow_selected_subview_reduction_value_dependent_selector1_output.txt" "selector=1"

append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_allocation_reduction" "mlir_baseline" "mlir_baseline" "$ALLOC_MLIR_BASELINE_SRC" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.ll" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_selector0_output.txt" "selector=0"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_allocation_reduction" "mlir_baseline" "mlir_baseline" "$ALLOC_MLIR_BASELINE_SRC" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline.ll" "$OUT_DIR/control_flow_selected_allocation_reduction_mlir_baseline_selector1_output.txt" "selector=1"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_allocation_reduction" "scair_baseline" "scair_baseline" "$ALLOC_BASELINE_SRC" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.ll" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_selector0_output.txt" "selector=0"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_allocation_reduction" "scair_baseline" "scair_baseline" "$ALLOC_BASELINE_SRC" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.llvm.mlir" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline.ll" "$OUT_DIR/control_flow_selected_allocation_reduction_baseline_selector1_output.txt" "selector=1"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_allocation_reduction" "value_dependent" "value_dependent" "$ALLOC_VALUE_DEP_SRC" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.llvm.mlir" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.ll" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_selector0_output.txt" "selector=0"
append_row "$SUMMARY_CSV" "$SUMMARY_MD" "control_flow_selected_allocation_reduction" "value_dependent" "value_dependent" "$ALLOC_VALUE_DEP_SRC" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.llvm.mlir" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.ll" "$OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_selector1_output.txt" "selector=1"

echo
echo "ScaIR memref-control-flow build complete."
echo "Produced:"
echo "  $OUT_DIR/control_flow_selected_subview_reduction_baseline_exec"
echo "  $OUT_DIR/control_flow_selected_subview_reduction_value_dependent_exec"
echo "  $OUT_DIR/control_flow_selected_subview_reduction_baseline.ll"
echo "  $OUT_DIR/control_flow_selected_subview_reduction_value_dependent.ll"
echo "  $OUT_DIR/control_flow_selected_allocation_reduction_baseline_exec"
echo "  $OUT_DIR/control_flow_selected_allocation_reduction_value_dependent_exec"
echo "  $OUT_DIR/control_flow_selected_allocation_reduction_baseline.ll"
echo "  $OUT_DIR/control_flow_selected_allocation_reduction_value_dependent.ll"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
