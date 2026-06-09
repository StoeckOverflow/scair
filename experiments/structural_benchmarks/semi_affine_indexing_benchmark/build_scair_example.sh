#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-clean-build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-100}"
SEMI_AFFINE_ROWS="${SEMI_AFFINE_ROWS:-256}"
SEMI_AFFINE_COLS="${SEMI_AFFINE_COLS:-1024}"
SEMI_AFFINE_STRIDE1="${SEMI_AFFINE_STRIDE1:-2}"
SEMI_AFFINE_STRIDE0="${SEMI_AFFINE_STRIDE0:-$((SEMI_AFFINE_COLS * SEMI_AFFINE_STRIDE1))}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_TRANSLATE="${MLIR_TRANSLATE:-$BIN_DIR/mlir-translate}"
MLIR_OPT="${MLIR_OPT:-$BIN_DIR/mlir-opt}"
MLIR_BASELINE_SRC="${MLIR_BASELINE_SRC:-$EXAMPLE_DIR/semi_affine_kernel_mlir_baseline.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"

BASELINE_SRC="${BASELINE_SRC:-$EXAMPLE_DIR/semi_affine_kernel_scair_baseline.mlir}"
BASELINE_DRIVER_SRC="${BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline_bare.c}"

VALUE_DEP_SRC="${VALUE_DEP_SRC:-$EXAMPLE_DIR/semi_affine_kernel_scair_value_dependent.mlir}"

VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver_bare.c}"
VALUE_DEP_LITERAL_DRIVER_SRC="${VALUE_DEP_LITERAL_DRIVER_SRC:-$EXAMPLE_DIR/driver_bare_literal_dims.c}"

OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
SIZE_DESCRIPTOR="rows=${SEMI_AFFINE_ROWS};cols=${SEMI_AFFINE_COLS};stride0=${SEMI_AFFINE_STRIDE0};stride1=${SEMI_AFFINE_STRIDE1};layout=semi_affine"
COMPILER_FLAGS="-O2"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$MLIR_BASELINE_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$BASELINE_SRC"
require_file "$VALUE_DEP_SRC"
require_file "$BASELINE_DRIVER_SRC"
require_file "$VALUE_DEP_DRIVER_SRC"
require_file "$VALUE_DEP_LITERAL_DRIVER_SRC"

value_dep_driver_for_src() {
  local src="$1"
  case "$(basename "$src")" in
    semi_affine_kernel_scair_value_dependent.mlir) echo "$VALUE_DEP_LITERAL_DRIVER_SRC" ;;
    *) echo "$VALUE_DEP_DRIVER_SRC" ;;
  esac
}

VALUE_DEP_DRIVER_EFFECTIVE="$(value_dep_driver_for_src "$VALUE_DEP_SRC")"

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

  "$SCAIR_OPT" -s "$src" --passes "$route,convert-func-to-llvm,convert-llvm-export-abi" \
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
  local variant="$3"
  local representation="$4"
  local src="$5"
  local lowered_mlir="$6"
  local llvm_ir="$7"
  local output_txt="$8"

  append_metrics_csv_row \
    "$metrics_csv" \
    "semi_affine_indexing_benchmark" \
    "semi_affine_fill_and_sum" \
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
    "$(count_source_d_memref_load_ops "$src")" \
    "$(count_source_d_memref_store_ops "$src")" \
    "$(count_func_defs "$lowered_mlir")" \
    "$(count_ops "$lowered_mlir")" \
    "$(count_ops_structural "$lowered_mlir")" \
    "$(file_metric lines "$lowered_mlir")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(count_llvm_calls "$llvm_ir")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "semi-affine layout benchmark" \
    "$(count_source_helpers "$src")" \
    "0" \
    "0" \
    "NA" \
    "NA" \
    "semi_affine" \
    "$SIZE_DESCRIPTOR" \
    "$variant" \
    "NA" \
    "NA" \
    "NA" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "$(metric_field runtime_iqr_ns_per_iter "$output_txt")" \
    "$(metric_field benchmark_repetitions "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field run_status "$output_txt")" \
    "$COMPILER_FLAGS" \
    "$GIT_COMMIT" \
    "$RUN_DATE" \
    "$MACHINE_ID" \
    "$ENV_PATH" \
    "$(metric_field raw_timings_path "$output_txt")"

  append_summary_row \
    "$summary_md" \
    "semi_affine_fill_and_sum" \
    "$variant" \
    "" \
    "$(metric_field build_status "$output_txt")" \
    "$(metric_field run_status "$output_txt")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$lowered_mlir")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

echo "==> Building upstream MLIR semi-affine baseline"
build_mlir_kernel \
  "$MLIR_BASELINE_SRC" \
  "$OUT_DIR/semi_affine_mlir_baseline.o" \
  "$OUT_DIR/semi_affine_mlir_baseline.ll" \
  "$OUT_DIR/semi_affine_mlir_baseline_metrics.txt"

echo "==> Linking upstream MLIR semi-affine baseline executable"
"$CC" -O2 \
  -DBENCH_LABEL="\"semi_affine_fill_and_sum\"" \
  -DVARIANT_LABEL="\"mlir_baseline\"" \
  "$MLIR_DRIVER_SRC" \
  "$OUT_DIR/semi_affine_mlir_baseline.o" \
  -o "$OUT_DIR/semi_affine_mlir_baseline_exec"
run_benchmark_repeated "$OUT_DIR/semi_affine_mlir_baseline_output.txt" \
  "$OUT_DIR/semi_affine_mlir_baseline_exec" "$ITERATIONS" \
  "$SEMI_AFFINE_STRIDE0" "$SEMI_AFFINE_STRIDE1" \
  "$SEMI_AFFINE_ROWS" "$SEMI_AFFINE_COLS"
cat "$OUT_DIR/semi_affine_mlir_baseline_metrics.txt" >> "$OUT_DIR/semi_affine_mlir_baseline_output.txt"

echo "==> Building ScaIR semi-affine baseline kernel-only"
build_kernel \
  "lower-dynamic-memref-to-llvm-baseline" \
  "$BASELINE_SRC" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.o" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.ll" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair_metrics.txt"

echo "==> Linking ScaIR semi-affine baseline kernel-only executable"
"$CC" -O2 \
  -DBENCH_LABEL="\"semi_affine_fill_and_sum\"" \
  -DVARIANT_LABEL="\"baseline\"" \
  "$BASELINE_DRIVER_SRC" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.o" \
  -o "$OUT_DIR/semi_affine_baseline_kernel_only_scair_exec"
run_benchmark_repeated "$OUT_DIR/semi_affine_baseline_kernel_only_scair_output.txt" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair_exec" "$ITERATIONS" \
  "$SEMI_AFFINE_STRIDE0" "$SEMI_AFFINE_STRIDE1" \
  "$SEMI_AFFINE_ROWS" "$SEMI_AFFINE_COLS"
cat "$OUT_DIR/semi_affine_baseline_kernel_only_scair_metrics.txt" >> "$OUT_DIR/semi_affine_baseline_kernel_only_scair_output.txt"

echo "==> Building ScaIR semi-affine value-dependent kernel-only"
build_kernel \
  "lower-d-memref-to-llvm" \
  "$VALUE_DEP_SRC" \
  "$OUT_DIR/semi_affine_value_dependent_scair.o" \
  "$OUT_DIR/semi_affine_value_dependent_scair.ll" \
  "$OUT_DIR/semi_affine_value_dependent_scair_metrics.txt"

echo "==> Linking ScaIR semi-affine value-dependent executable"
"$CC" -O2 \
  -DBENCH_LABEL="\"semi_affine_fill_and_sum\"" \
  -DVARIANT_LABEL="\"value_dependent\"" \
  "$VALUE_DEP_DRIVER_EFFECTIVE" \
  "$OUT_DIR/semi_affine_value_dependent_scair.o" \
  -o "$OUT_DIR/semi_affine_value_dependent_scair_exec"
run_benchmark_repeated "$OUT_DIR/semi_affine_value_dependent_scair_output.txt" \
  "$OUT_DIR/semi_affine_value_dependent_scair_exec" "$ITERATIONS" \
  "$SEMI_AFFINE_STRIDE0" "$SEMI_AFFINE_STRIDE1" \
  "$SEMI_AFFINE_ROWS" "$SEMI_AFFINE_COLS"
cat "$OUT_DIR/semi_affine_value_dependent_scair_metrics.txt" >> "$OUT_DIR/semi_affine_value_dependent_scair_output.txt"

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Semi-Affine Indexing Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

append_row \
  "$SUMMARY_CSV" \
  "$SUMMARY_MD" \
  "mlir_baseline" \
  "mlir_baseline" \
  "$MLIR_BASELINE_SRC" \
  "$OUT_DIR/semi_affine_mlir_baseline.llvm.mlir" \
  "$OUT_DIR/semi_affine_mlir_baseline.ll" \
  "$OUT_DIR/semi_affine_mlir_baseline_output.txt"
append_row \
  "$SUMMARY_CSV" \
  "$SUMMARY_MD" \
  "scair_baseline" \
  "scair_baseline" \
  "$BASELINE_SRC" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.llvm.mlir" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.ll" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair_output.txt"
append_row \
  "$SUMMARY_CSV" \
  "$SUMMARY_MD" \
  "value_dependent" \
  "value_dependent" \
  "$VALUE_DEP_SRC" \
  "$OUT_DIR/semi_affine_value_dependent_scair.llvm.mlir" \
  "$OUT_DIR/semi_affine_value_dependent_scair.ll" \
  "$OUT_DIR/semi_affine_value_dependent_scair_output.txt"

echo
echo "ScaIR semi-affine build complete."
echo "Produced:"
echo "  $OUT_DIR/semi_affine_mlir_baseline_exec"
echo "  $OUT_DIR/semi_affine_baseline_kernel_only_scair_exec"
echo "  $OUT_DIR/semi_affine_value_dependent_scair_exec"
echo "  $OUT_DIR/semi_affine_mlir_baseline.ll"
echo "  $OUT_DIR/semi_affine_baseline_kernel_only_scair.ll"
echo "  $OUT_DIR/semi_affine_value_dependent_scair.ll"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
