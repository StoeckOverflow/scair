#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$ROOT/experiments/design_benchmarks/shape_reification_benchmark"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
SCAIR_OPT="${SCAIR_OPT:-$ROOT/out/tools/opt/launcher.dest/run}"
LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-clean-build}"
MLIR_OPT="${MLIR_OPT:-$LLVM_BUILD_DIR/bin/mlir-opt}"

ORD_IDENTICAL="$EXAMPLE_DIR/ordinary_dynamic_shape_identical_ssa.mlir"
ORD_DIFFERENT="$EXAMPLE_DIR/ordinary_dynamic_shape_same_shape_different_ssa.mlir"
DEP_SOURCE="$EXAMPLE_DIR/dependent_shape.mlir"
METRICS="$OUT_DIR/metrics.csv"
SUMMARY="$OUT_DIR/summary.md"

mkdir -p "$OUT_DIR"

require_file() {
  if [[ ! -f "$1" ]]; then
    echo "error: required file not found: $1" >&2
    exit 1
  fi
}

require_exe() {
  if [[ ! -x "$1" ]]; then
    echo "error: required executable not found: $1" >&2
    exit 1
  fi
}

count_pattern() {
  local path="$1"
  local pattern="$2"
  local matches
  matches="$(rg -o "$pattern" "$path" || true)"
  if [[ -z "$matches" ]]; then
    echo 0
  else
    printf '%s\n' "$matches" | wc -l | tr -d ' '
  fi
}

line_count() {
  wc -l < "$1" | tr -d ' '
}

total_op_count() {
  count_pattern "$1" '^[[:space:]]*(%[A-Za-z0-9_]+(:[0-9]+)? = )?("[A-Za-z0-9_\.]+"|[A-Za-z_][A-Za-z0-9_]*\.[A-Za-z0-9_\.]*)'
}

arith_index_arithmetic_count() {
  count_pattern "$1" '(^|[^A-Za-z0-9_])"?arith\.(addi|muli|subi|divsi|divui|ceildivsi|floordivsi)"?'
}

allocation_shape_op_count() {
  count_pattern "$1" '(^|[^A-Za-z0-9_])"?(tensor\.empty|memref\.alloc|d_tensor\.empty)"?'
}

metric_row() {
  local variant="$1"
  local stage="$2"
  local path="$3"
  local input_ops="$4"
  local toolchain="$5"
  local status="$6"
  local notes="$7"
  local ops
  ops="$(total_op_count "$path")"
  local tensor_dim
  local memref_dim
  local d_tensor_dim
  local d_memref_dim
  local shape_to_index
  local shape_ops
  local casts
  local arith_index
  local alloc_shape
  tensor_dim="$(count_pattern "$path" '(^|[^A-Za-z0-9_])tensor\.dim')"
  memref_dim="$(count_pattern "$path" '(^|[^A-Za-z0-9_])memref\.dim')"
  d_tensor_dim="$(count_pattern "$path" 'd_tensor\.dim')"
  d_memref_dim="$(count_pattern "$path" 'd_memref\.dim')"
  shape_to_index="$(count_pattern "$path" 'd_tensor\.shape\.to_index')"
  shape_ops="$(count_pattern "$path" '(^|[^A-Za-z0-9_\.])shape\.[A-Za-z_]+')"
  casts="$(count_pattern "$path" 'builtin\.unrealized_conversion_cast')"
  arith_index="$(arith_index_arithmetic_count "$path")"
  alloc_shape="$(allocation_shape_op_count "$path")"
  local shape_management_ops=$((tensor_dim + memref_dim + d_tensor_dim + d_memref_dim + shape_to_index + shape_ops + casts + arith_index + alloc_shape))
  local delta="NA"
  if [[ "$input_ops" != "NA" ]]; then
    delta=$((input_ops - ops))
  fi
  local ratio="NA"
  if [[ "${ORDINARY_SAME_SHAPE_FINAL_OPS:-NA}" != "NA" && "${ORDINARY_SAME_SHAPE_FINAL_OPS:-0}" != "0" ]]; then
    ratio="$(awk -v ops="$ops" -v base="$ORDINARY_SAME_SHAPE_FINAL_OPS" 'BEGIN { printf "%.3f", ops / base }')"
  fi
  {
    printf '%s,%s,%s,%s,%s,' "$variant" "$stage" "$toolchain" "$status" "$(basename "$path")"
    printf '%s,' "$tensor_dim"
    printf '%s,' "$memref_dim"
    printf '%s,' "$d_tensor_dim"
    printf '%s,' "$d_memref_dim"
    printf '%s,' "$shape_to_index"
    printf '%s,' "$(count_pattern "$path" 'd_tensor\.nat\.[A-Za-z_]+')"
    printf '%s,' "$shape_ops"
    printf '%s,' "$casts"
    printf '%s,' "$(count_pattern "$path" 'arith\.constant.*index|arith\.constant_index')"
    printf '%s,' "$arith_index"
    printf '%s,' "$alloc_shape"
    printf '%s,' "$shape_management_ops"
    printf '%s,%s,%s,%s,' "$ops" "$(line_count "$path")" "$delta" "$ratio"
    printf '%s\n' "$notes"
  } >> "$METRICS"
}

run_mlir_pipeline() {
  local input="$1"
  local output="$2"
  shift 2
  "$MLIR_OPT" "$input" "$@" > "$output"
}

run_scair_pipeline() {
  local input="$1"
  local output="$2"
  local passes="$3"
  "$SCAIR_OPT" "$input" --allow-unregistered-dialect -p "$passes" > "$output"
}

require_file "$ORD_IDENTICAL"
require_file "$ORD_DIFFERENT"
require_file "$DEP_SOURCE"
require_exe "$SCAIR_OPT"
require_exe "$MLIR_OPT"

cat > "$METRICS" <<'CSV'
variant,stage,toolchain,status,artifact,tensor_dim_count,memref_dim_count,d_tensor_dim_count,d_memref_dim_count,d_tensor_shape_to_index_count,d_tensor_nat_op_count,shape_op_count,unrealized_cast_count,arith_constant_index_count,arith_index_arithmetic_count,allocation_shape_op_count,shape_management_op_count,total_op_count,mlir_loc,removed_op_delta,total_op_ratio_vs_ordinary_same_shape_final,notes
CSV

process_ordinary() {
  local variant="$1"
  local input="$2"
  local prefix="$OUT_DIR/$variant"
  local raw="$prefix.input.mlir"
  local canonical="$prefix.stock_canonicalize_cse.mlir"
  local resolved="$prefix.stock_resolve_dims_canonicalize_cse.mlir"
  local reified="$prefix.stock_reify_shapes_canonicalize_cse.mlir"
  cp "$input" "$raw"
  local input_ops
  input_ops="$(total_op_count "$raw")"
  run_mlir_pipeline "$raw" "$canonical" --canonicalize --cse --symbol-dce
  run_mlir_pipeline "$raw" "$resolved" --resolve-shaped-type-result-dims --canonicalize --cse --symbol-dce
  run_mlir_pipeline "$raw" "$reified" --reify-result-shapes --canonicalize --cse --symbol-dce
  if [[ "$variant" == "ordinary_dynamic_shape_same_shape_different_ssa" ]]; then
    ORDINARY_SAME_SHAPE_FINAL_OPS="$(total_op_count "$canonical")"
  fi

  metric_row "$variant" "input" "$raw" "$input_ops" "upstream_mlir" "ok" "baseline_input"
  metric_row "$variant" "after_canonicalize_cse" "$canonical" "$input_ops" "upstream_mlir" "ok" "canonicalize+cse+symbol-dce"
  metric_row "$variant" "after_resolve_shaped_type_result_dims" "$resolved" "$input_ops" "upstream_mlir" "ok" "resolve-shaped-type-result-dims+canonicalize+cse+symbol-dce"
  metric_row "$variant" "after_reify_result_shapes" "$reified" "$input_ops" "upstream_mlir" "ok" "reify-result-shapes+canonicalize+cse+symbol-dce"
}

process_dependent() {
  local prefix="$OUT_DIR/dependent_shape"
  local raw="$prefix.input.mlir"
  local no_elim_cleanup="$prefix.no_elim_cleanup.mlir"
  local after_elim="$prefix.after_dim_elim.mlir"
  local after_cleanup="$prefix.after_dim_elim_cleanup.mlir"
  cp "$DEP_SOURCE" "$raw"
  local input_ops
  input_ops="$(total_op_count "$raw")"

  run_scair_pipeline "$raw" "$no_elim_cleanup" "reconcile-unrealized-casts,canonicalize,cse,dce"
  run_scair_pipeline "$raw" "$after_elim" "dependent-dim-query-elim"
  run_scair_pipeline "$raw" "$after_cleanup" "dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce"

  metric_row "dependent_shape_no_elim" "input" "$raw" "$input_ops" "scair" "ok" "dependent_shape_provenance_input"
  metric_row "dependent_shape_no_elim" "after_cleanup_no_elim" "$no_elim_cleanup" "$input_ops" "scair" "ok" "reconcile-unrealized-casts+canonicalize+cse+dce_without_dim_elim"
  metric_row "dependent_shape_dim_elim" "input" "$raw" "$input_ops" "scair" "ok" "dependent_shape_provenance_input"
  metric_row "dependent_shape_dim_elim" "after_dim_elim" "$after_elim" "$input_ops" "scair" "ok" "dependent-dim-query-elim"
  metric_row "dependent_shape_dim_elim" "after_cleanup" "$after_cleanup" "$input_ops" "scair" "ok" "dependent-dim-query-elim+reconcile-unrealized-casts+canonicalize+cse+dce"
}

process_ordinary "ordinary_dynamic_shape_same_shape_different_ssa" "$ORD_DIFFERENT"
process_ordinary "ordinary_dynamic_shape_identical_ssa" "$ORD_IDENTICAL"
process_dependent

{
  echo "# Shape Reification Benchmark Summary"
  echo
  echo "Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo
  echo "| Variant | Stage | tensor.dim | d_tensor.dim | shape.to_index | index arith | shape mgmt ops | total ops | LOC | Removed delta | Ratio | Notes |"
  echo "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
  tail -n +2 "$METRICS" | while IFS=, read -r variant stage toolchain status artifact tensor_dim memref_dim d_tensor_dim d_memref_dim shape_to_index nat_ops shape_ops casts arith_const arith_index alloc_shape shape_mgmt ops loc delta ratio notes; do
    echo "| \`$variant\` | \`$stage\` | $tensor_dim | $d_tensor_dim | $shape_to_index | $arith_index | $shape_mgmt | $ops | $loc | $delta | $ratio | $notes |"
  done
  echo
  echo "Key comparison:"
  echo "- The ordinary identical-SSA case shows the fair baseline where upstream CSE can merge repeated syntactically identical \`tensor.dim\` queries."
  echo "- The ordinary same-shaped/different-SSA case keeps separate \`tensor.dim\` queries and repeated \`m*n\` size arithmetic because the equality contract is not represented in stock tensor types."
  echo "- The dependent route carries \`%m/%n\` in the tensor type, so \`dependent-dim-query-elim\` rewrites all repeated \`d_tensor.dim\` queries to shared nat provenance before ordinary cleanup runs."
  echo "- After cleanup, the dependent route should retain two \`shape.to_index\` materializations and one shared \`m*n\` computation for the fanout chain."
} > "$SUMMARY"

echo "Shape reification benchmark complete."
echo "Produced:"
echo "  $METRICS"
echo "  $SUMMARY"
