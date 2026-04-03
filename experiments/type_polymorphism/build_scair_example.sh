#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-2000000}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="${BIN_DIR}/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
DRIVER_TEMPLATE="${EXAMPLE_DIR}/driver_template.c"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/build_scair}"

BENCHMARKS=(
  "compose_fanout"
  "higher_order_accumulator"
  "batched_map_small"
)

mkdir -p "$OUT_DIR"

require_file() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "error: missing file: $path" >&2
    exit 1
  fi
}

require_bin() {
  local path="$1"
  if [[ ! -x "$path" ]]; then
    echo "error: missing executable: $path" >&2
    exit 1
  fi
}

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$DRIVER_TEMPLATE"

now_ns() {
  date +%s%N
}

format_ms() {
  local start_ns="$1"
  local end_ns="$2"
  awk -v start="$start_ns" -v end="$end_ns" 'BEGIN { printf "%.2f", (end - start) / 1000000.0 }'
}

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
      echo "error: unknown ScaIR variant: $variant" >&2
      exit 1
      ;;
  esac
}

count_matches() {
  local pattern="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count=$(rg -o "$pattern" "$path" 2>/dev/null | wc -l | tr -d ' ')
  echo "${count:-0}"
}

count_ops() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count=$(
    rg '^[[:space:]]*(%[^=[:space:]]+[[:space:]]*=.*|call @|return([[:space:]]|$)|func\.return|llvm\.call|func\.call|"[^"]+")' "$path" \
      | wc -l | tr -d ' '
  )
  echo "${count:-0}"
}

count_source_helpers() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local funcs
  funcs=$(rg '^[[:space:]]*func\.func @' "$path" | wc -l | tr -d ' ')
  if [[ -z "$funcs" || "$funcs" -le 1 ]]; then
    echo "0"
  else
    echo $((funcs - 1))
  fi
}

count_lowered_funcs() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count=$(rg '^[[:space:]]*func\.func @' "$path" | wc -l | tr -d ' ')
  echo "${count:-0}"
}

metric_field() {
  local key="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  awk -F= -v target="$key" '$1 == target { print $2 }' "$path"
}

file_metric() {
  local mode="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  case "$mode" in
    bytes)
      wc -c < "$path" | tr -d ' '
      ;;
    lines)
      wc -l < "$path" | tr -d ' '
      ;;
    *)
      echo "NA"
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

  local start_ns
  local end_ns
  start_ns=$(now_ns)

  local build_log="$OUT_DIR/${prefix}_build.log"
  local status=0
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
      "$DRIVER_TEMPLATE" "$obj" \
      -o "$exe" 2>> "$build_log"
    status=$?
  fi
  set -e

  end_ns=$(now_ns)
  if [[ $status -eq 0 ]]; then
    "$exe" "$ITERATIONS" > "$output_txt"
    {
      echo "build_status=ok"
      printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
    } >> "$output_txt"
  else
    {
      echo "build_status=unsupported"
      printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
      echo "result=NA"
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
    "$DRIVER_TEMPLATE" "$obj" \
    -o "$exe"

  end_ns=$(now_ns)
  "$exe" "$ITERATIONS" > "$output_txt"
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } >> "$output_txt"
}

metric_row_md() {
  local bench="$1"
  local variant="$2"
  local src="$3"
  local lowered_func="$4"
  local llvm_ir="$5"
  local opt_llvm_ir="$6"
  local output_txt="$7"
  local debruijn_refs="$8"
  local ssa_type_refs="$9"

  printf "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n" \
    "$bench" \
    "$variant" \
    "$(file_metric bytes "$src")" \
    "$(file_metric lines "$src")" \
    "$(count_ops "$src")" \
    "$(count_source_helpers "$src")" \
    "$debruijn_refs" \
    "$ssa_type_refs" \
    "$(count_lowered_funcs "$lowered_func")" \
    "$(count_ops "$lowered_func")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(count_matches ' call ' "$llvm_ir")" \
    "$(file_metric lines "$opt_llvm_ir")" \
    "$(count_matches ' call ' "$opt_llvm_ir")" \
    "$(metric_field build_status "$output_txt")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

metric_row_csv() {
  local bench="$1"
  local variant="$2"
  local src="$3"
  local lowered_func="$4"
  local llvm_ir="$5"
  local opt_llvm_ir="$6"
  local output_txt="$7"
  local debruijn_refs="$8"
  local ssa_type_refs="$9"

  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "$bench" \
    "$variant" \
    "$(file_metric bytes "$src")" \
    "$(file_metric lines "$src")" \
    "$(count_ops "$src")" \
    "$(count_source_helpers "$src")" \
    "$debruijn_refs" \
    "$ssa_type_refs" \
    "$(count_lowered_funcs "$lowered_func")" \
    "$(count_ops "$lowered_func")" \
    "$(file_metric lines "$llvm_ir")" \
    "$(count_matches ' call ' "$llvm_ir")" \
    "$(file_metric lines "$opt_llvm_ir")" \
    "$(count_matches ' call ' "$opt_llvm_ir")" \
    "$(metric_field build_status "$output_txt")" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field result "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")"
}

for bench in "${BENCHMARKS[@]}"; do
  echo "==> Building ${bench} de Bruijn baseline"
  baseline_src="$(source_path "$bench" "baseline_de_bruijn")"
  require_file "$baseline_src"
  build_scair_program "$bench" "baseline_de_bruijn" "$baseline_src" "$(pipeline_for_variant "baseline_de_bruijn")"

  echo "==> Building ${bench} value-dependent variant"
  value_src="$(source_path "$bench" "value_dependent")"
  require_file "$value_src"
  build_scair_program "$bench" "value_dependent" "$value_src" "$(pipeline_for_variant "value_dependent")"

  echo "==> Building ${bench} upstream MLIR baseline"
  mlir_src="$(source_path "$bench" "mlir_baseline")"
  require_file "$mlir_src"
  build_mlir_program "$bench" "$mlir_src"
done

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
{
  cat <<'EOF'
# Type Polymorphism Design Benchmark Summary

These are design benchmarks. The target comparison is not "which path produces a dramatically faster final kernel" but:

- how much monomorphic duplication the plain MLIR baseline needs
- how direct the value-dependent ScaIR encoding is relative to the de Bruijn baseline
- whether both ScaIR encodings converge to comparable lowered kernels

| Benchmark | Variant | Source bytes | Source LOC | Source ops | Source helper defs | bvar refs | value SSA refs | Lowered func defs | Lowered func ops | LLVM IR lines | LLVM call count | O2 LLVM lines | O2 LLVM call count | Build status | Compile ms | Result | ns/iter |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: | ---: |
EOF
  for bench in "${BENCHMARKS[@]}"; do
    metric_row_md \
      "$bench" \
      "baseline_de_bruijn" \
      "$(source_path "$bench" "baseline_de_bruijn")" \
      "$OUT_DIR/${bench}_baseline_de_bruijn_lowered_func.mlir" \
      "$OUT_DIR/${bench}_baseline_de_bruijn.ll" \
      "$OUT_DIR/${bench}_baseline_de_bruijn.opt.ll" \
      "$OUT_DIR/${bench}_baseline_de_bruijn_output.txt" \
      "$(count_matches 'bvar<' "$(source_path "$bench" "baseline_de_bruijn")")" \
      "0"
    metric_row_md \
      "$bench" \
      "value_dependent" \
      "$(source_path "$bench" "value_dependent")" \
      "$OUT_DIR/${bench}_value_dependent_lowered_func.mlir" \
      "$OUT_DIR/${bench}_value_dependent.ll" \
      "$OUT_DIR/${bench}_value_dependent.opt.ll" \
      "$OUT_DIR/${bench}_value_dependent_output.txt" \
      "0" \
      "$(count_matches 'value<%' "$(source_path "$bench" "value_dependent")")"
    metric_row_md \
      "$bench" \
      "mlir_baseline" \
      "$(source_path "$bench" "mlir_baseline")" \
      "$OUT_DIR/${bench}_mlir_baseline_lowered_func.mlir" \
      "$OUT_DIR/${bench}_mlir_baseline.ll" \
      "$OUT_DIR/${bench}_mlir_baseline.opt.ll" \
      "$OUT_DIR/${bench}_mlir_baseline_output.txt" \
      "0" \
      "0"
  done
 } > "$SUMMARY_MD"

{
  echo "benchmark,variant,source_bytes,source_loc,source_ops,source_helper_defs,bvar_refs,value_ssa_refs,lowered_func_defs,lowered_func_ops,llvm_ir_lines,llvm_call_count,opt_llvm_lines,opt_llvm_call_count,build_status,compile_ms,result,ns_per_iter"
  for bench in "${BENCHMARKS[@]}"; do
    metric_row_csv \
      "$bench" \
      "baseline_de_bruijn" \
      "$(source_path "$bench" "baseline_de_bruijn")" \
      "$OUT_DIR/${bench}_baseline_de_bruijn_lowered_func.mlir" \
      "$OUT_DIR/${bench}_baseline_de_bruijn.ll" \
      "$OUT_DIR/${bench}_baseline_de_bruijn.opt.ll" \
      "$OUT_DIR/${bench}_baseline_de_bruijn_output.txt" \
      "$(count_matches 'bvar<' "$(source_path "$bench" "baseline_de_bruijn")")" \
      "0"
    metric_row_csv \
      "$bench" \
      "value_dependent" \
      "$(source_path "$bench" "value_dependent")" \
      "$OUT_DIR/${bench}_value_dependent_lowered_func.mlir" \
      "$OUT_DIR/${bench}_value_dependent.ll" \
      "$OUT_DIR/${bench}_value_dependent.opt.ll" \
      "$OUT_DIR/${bench}_value_dependent_output.txt" \
      "0" \
      "$(count_matches 'value<%' "$(source_path "$bench" "value_dependent")")"
    metric_row_csv \
      "$bench" \
      "mlir_baseline" \
      "$(source_path "$bench" "mlir_baseline")" \
      "$OUT_DIR/${bench}_mlir_baseline_lowered_func.mlir" \
      "$OUT_DIR/${bench}_mlir_baseline.ll" \
      "$OUT_DIR/${bench}_mlir_baseline.opt.ll" \
      "$OUT_DIR/${bench}_mlir_baseline_output.txt" \
      "0" \
      "0"
  done
} > "$SUMMARY_CSV"

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
