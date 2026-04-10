#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-2000000}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="${BIN_DIR}/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
DRIVER_TEMPLATE="${EXAMPLE_DIR}/driver_template.c"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/build_scair}"

BENCHMARKS=(
  "shared_polymorphic_identity_multitype"
  "shared_polymorphic_kernel_bank_multitype"
)

mkdir -p "$OUT_DIR"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$DRIVER_TEMPLATE"

safe_name() {
  local bench="$1"
  local variant="$2"
  echo "${bench}_${variant}"
}

source_path() {
  local bench="$1"
  local variant="$2"
  echo "$EXAMPLE_DIR/${variant}_${bench}.mlir"
}

pipeline_for_variant() {
  local variant="$1"
  case "$variant" in
    baseline_de_bruijn)
      echo "monomorphize-tlam-de-bruijn,beta-reduce-tlam-de-bruijn,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func"
      ;;
    value_dependent)
      echo "monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func"
      ;;
    *)
      echo "error: unknown variant: $variant" >&2
      exit 1
      ;;
  esac
}

representation_group_for_variant() {
  case "$1" in
    baseline_de_bruijn) echo "scair_baseline" ;;
    value_dependent) echo "value_dependent" ;;
    mlir_baseline) echo "mlir_baseline" ;;
    *)
      echo "error: unknown variant: $1" >&2
      exit 1
      ;;
  esac
}

report_variant_name() {
  case "$1" in
    baseline_de_bruijn) echo "debruijn" ;;
    value_dependent) echo "value_dependent" ;;
    mlir_baseline) echo "mlir_baseline" ;;
    *)
      echo "error: unknown variant: $1" >&2
      exit 1
      ;;
  esac
}

expected_result_for_bench() {
  case "$1" in
    shared_polymorphic_identity_multitype) echo "29" ;;
    shared_polymorphic_kernel_bank_multitype) echo "3090" ;;
    *)
      echo "error: unknown benchmark: $1" >&2
      exit 1
      ;;
  esac
}

build_scair_program() {
  local bench="$1"
  local variant="$2"
  local src="$3"
  local poly_pipeline="$4"
  local prefix
  prefix="$(safe_name "$bench" "$variant")"

  local lowered_func="$OUT_DIR/${prefix}_lowered_func.mlir"
  local lowered_llvm="$OUT_DIR/${prefix}_llvm.mlir"
  local llvm_ir="$OUT_DIR/${prefix}.ll"
  local opt_llvm_ir="$OUT_DIR/${prefix}.opt.ll"
  local obj="$OUT_DIR/${prefix}.o"
  local exe="$OUT_DIR/${prefix}_exec"
  local output_txt="$OUT_DIR/${prefix}_output.txt"
  local build_log="$OUT_DIR/${prefix}_build.log"

  local start_ns
  local end_ns
  local status=0
  start_ns=$(now_ns)

  set +e
  "$SCAIR_OPT" "$src" --allow-unregistered-dialect \
    --passes "$poly_pipeline,canonicalize,cse" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\\[[0-9.]+s\\]\\[warning\\]\\[perf,memops\\] Cannot use file /tmp/hsperfdata_)' \
    > "$lowered_func" 2> "$build_log"
  status=$?
  if [[ $status -eq 0 ]]; then
    "$MLIR_OPT" "$lowered_func" \
      --pass-pipeline="builtin.module(func.func(convert-arith-to-llvm),convert-func-to-llvm,reconcile-unrealized-casts)" \
      > "$lowered_llvm" 2>> "$build_log"
    status=$?
  fi
  if [[ $status -eq 0 ]]; then
    "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_llvm" > "$llvm_ir" 2>> "$build_log"
    status=$?
  fi
  if [[ $status -eq 0 ]]; then
    "$CC" -O2 -S -emit-llvm -x ir "$llvm_ir" -o "$opt_llvm_ir" 2>> "$build_log"
    status=$?
  fi
  if [[ $status -eq 0 ]]; then
    "$CC" -O2 -c -x ir "$llvm_ir" -o "$obj" 2>> "$build_log"
    status=$?
  fi
  if [[ $status -eq 0 ]]; then
    "$CC" -O2 \
      -DBENCH_FN="$bench" \
      -DBENCH_LABEL="\"$bench\"" \
      -DVARIANT_LABEL="\"$(report_variant_name "$variant")\"" \
      -DEXPECTED_RESULT="$(expected_result_for_bench "$bench")" \
      "$DRIVER_TEMPLATE" "$obj" \
      -o "$exe" 2>> "$build_log"
    status=$?
  fi
  set -e

  end_ns=$(now_ns)
  if [[ $status -eq 0 ]]; then
    run_benchmark_repeated "$output_txt" "$exe" "$ITERATIONS"
    {
      echo "build_status=ok"
      printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
    } >> "$output_txt"
  else
    {
      echo "benchmark=$bench"
      echo "variant=$(report_variant_name "$variant")"
      echo "build_status=unsupported"
      echo "run_status=NA"
      printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
      echo "result=NA"
      echo "expected_result=$(expected_result_for_bench "$bench")"
      echo "runtime_ns_per_iter=NA"
      echo "ns_per_iter=NA"
    } > "$output_txt"
  fi
}

build_mlir_program() {
  local bench="$1"
  local src="$2"
  local prefix
  prefix="$(safe_name "$bench" "mlir_baseline")"

  local lowered_func="$OUT_DIR/${prefix}_lowered_func.mlir"
  local lowered_llvm="$OUT_DIR/${prefix}_llvm.mlir"
  local llvm_ir="$OUT_DIR/${prefix}.ll"
  local opt_llvm_ir="$OUT_DIR/${prefix}.opt.ll"
  local obj="$OUT_DIR/${prefix}.o"
  local exe="$OUT_DIR/${prefix}_exec"
  local output_txt="$OUT_DIR/${prefix}_output.txt"

  local start_ns
  local end_ns
  start_ns=$(now_ns)

  cp "$src" "$lowered_func"
  "$MLIR_OPT" "$src" \
    --pass-pipeline="builtin.module(func.func(convert-arith-to-llvm),convert-func-to-llvm,reconcile-unrealized-casts)" \
    > "$lowered_llvm"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_llvm" > "$llvm_ir"
  "$CC" -O2 -S -emit-llvm -x ir "$llvm_ir" -o "$opt_llvm_ir"
  "$CC" -O2 -c -x ir "$llvm_ir" -o "$obj"
  "$CC" -O2 \
    -DBENCH_FN="$bench" \
    -DBENCH_LABEL="\"$bench\"" \
    -DVARIANT_LABEL="\"mlir_baseline\"" \
    -DEXPECTED_RESULT="$(expected_result_for_bench "$bench")" \
    "$DRIVER_TEMPLATE" "$obj" \
    -o "$exe"

  end_ns=$(now_ns)
  run_benchmark_repeated "$output_txt" "$exe" "$ITERATIONS"
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } >> "$output_txt"
}

append_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local bench="$3"
  local variant="$4"
  local src="$5"
  local lowered_func="$6"
  local llvm_ir="$7"
  local opt_llvm_ir="$8"
  local output_txt="$9"
  local notes="${10}"

  local representation
  representation="$(representation_group_for_variant "$variant")"
  local report_variant
  report_variant="$(report_variant_name "$variant")"
  local source_helpers
  local bvar_refs
  local value_refs

  if [[ "$variant" == "baseline_de_bruijn" ]]; then
    source_helpers="$(count_source_helpers "$src")"
    bvar_refs="$(count_matches 'bvar<' "$src")"
    value_refs="0"
  elif [[ "$variant" == "value_dependent" ]]; then
    source_helpers="$(count_source_helpers "$src")"
    bvar_refs="0"
    value_refs="$(count_matches 'value<%' "$src")"
  else
    source_helpers="$(count_source_helpers "$src")"
    bvar_refs="0"
    value_refs="0"
  fi

  append_metrics_csv_row \
    "$metrics_csv" \
    "type_polymorphism" \
    "$bench" \
    "$report_variant" \
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
    "$(count_func_defs "$lowered_func")" \
    "$(count_ops "$lowered_func")" \
    "$(count_ops_structural "$lowered_func")" \
    "$(file_metric lines "$lowered_func")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(count_llvm_calls "$llvm_ir")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "$notes" \
    "$source_helpers" \
    "$bvar_refs" \
    "$value_refs" \
    "$(file_metric lines "$opt_llvm_ir")" \
    "$(count_llvm_calls "$opt_llvm_ir")"

  append_summary_row \
    "$summary_md" \
    "$bench" \
    "$report_variant" \
    "" \
    "$(metric_field build_status "$output_txt")" \
    "$(metric_field run_status "$output_txt")" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$lowered_func")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field expected_result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

for bench in "${BENCHMARKS[@]}"; do
  echo "==> Building $bench de Bruijn baseline"
  baseline_src="$(source_path "$bench" "baseline_de_bruijn")"
  require_file "$baseline_src"
  build_scair_program "$bench" "baseline_de_bruijn" "$baseline_src" "$(pipeline_for_variant "baseline_de_bruijn")"

  echo "==> Building $bench value-dependent variant"
  value_src="$(source_path "$bench" "value_dependent")"
  require_file "$value_src"
  build_scair_program "$bench" "value_dependent" "$value_src" "$(pipeline_for_variant "value_dependent")"

  echo "==> Building $bench upstream MLIR baseline"
  mlir_src="$(source_path "$bench" "mlir_baseline")"
  require_file "$mlir_src"
  build_mlir_program "$bench" "$mlir_src"
done

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
write_summary_header "$SUMMARY_MD" "Type Polymorphism Design Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"

for bench in "${BENCHMARKS[@]}"; do
  append_row \
    "$SUMMARY_CSV" \
    "$SUMMARY_MD" \
    "$bench" \
    "baseline_de_bruijn" \
    "$(source_path "$bench" "baseline_de_bruijn")" \
    "$OUT_DIR/${bench}_baseline_de_bruijn_lowered_func.mlir" \
    "$OUT_DIR/${bench}_baseline_de_bruijn.ll" \
    "$OUT_DIR/${bench}_baseline_de_bruijn.opt.ll" \
    "$OUT_DIR/${bench}_baseline_de_bruijn_output.txt" \
    "de_bruijn baseline"
  append_row \
    "$SUMMARY_CSV" \
    "$SUMMARY_MD" \
    "$bench" \
    "value_dependent" \
    "$(source_path "$bench" "value_dependent")" \
    "$OUT_DIR/${bench}_value_dependent_lowered_func.mlir" \
    "$OUT_DIR/${bench}_value_dependent.ll" \
    "$OUT_DIR/${bench}_value_dependent.opt.ll" \
    "$OUT_DIR/${bench}_value_dependent_output.txt" \
    "value-dependent polymorphism"
  append_row \
    "$SUMMARY_CSV" \
    "$SUMMARY_MD" \
    "$bench" \
    "mlir_baseline" \
    "$(source_path "$bench" "mlir_baseline")" \
    "$OUT_DIR/${bench}_mlir_baseline_lowered_func.mlir" \
    "$OUT_DIR/${bench}_mlir_baseline.ll" \
    "$OUT_DIR/${bench}_mlir_baseline.opt.ll" \
    "$OUT_DIR/${bench}_mlir_baseline_output.txt" \
    "monomorphic MLIR baseline"
done

echo
echo "Type polymorphism design benchmark build complete."
echo "Produced:"
for bench in "${BENCHMARKS[@]}"; do
  echo "  $OUT_DIR/${bench}_baseline_de_bruijn_exec"
  echo "  $OUT_DIR/${bench}_value_dependent_exec"
  echo "  $OUT_DIR/${bench}_mlir_baseline_exec"
done
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/metrics.csv"
