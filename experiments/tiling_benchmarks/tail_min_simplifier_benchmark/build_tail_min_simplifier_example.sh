#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$ROOT/experiments/tiling_benchmarks/tail_min_simplifier_benchmark"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
SCAIR_OPT="${SCAIR_OPT:-$ROOT/out/tools/opt/launcher.dest/run}"
LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-clean-build}"
MLIR_OPT="${MLIR_OPT:-$LLVM_BUILD_DIR/bin/mlir-opt}"

DEPENDENT_SOURCE="$EXAMPLE_DIR/dependent_product_loop.mlir"
ORDINARY_SOURCE="$EXAMPLE_DIR/ordinary_product_loop.mlir"
STOCK_AFFINE_SOURCE="$EXAMPLE_DIR/stock_affine_product_loop.mlir"
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

tail_guard_count() {
  local path="$1"
  local arith_min
  local affine_min
  local d_affine_min
  arith_min="$(count_pattern "$path" 'arith\.min(si|ui)')"
  affine_min="$(count_pattern "$path" 'to min #map|affine\.min')"
  d_affine_min="$(count_pattern "$path" 'd_affine\.min')"
  echo $((arith_min + affine_min + d_affine_min))
}

metric_row() {
  local variant="$1"
  local stage="$2"
  local toolchain="$3"
  local status="$4"
  local path="$5"
  local base_ops="$6"
  local notes="$7"
  local ops
  ops="$(total_op_count "$path")"
  local delta="NA"
  if [[ "$base_ops" != "NA" ]]; then
    delta=$((base_ops - ops))
  fi

  {
    printf '%s,%s,%s,%s,%s,' "$variant" "$stage" "$toolchain" "$status" "$(basename "$path")"
    printf '%s,' "$(count_pattern "$path" 'to min #map|affine\.min')"
    printf '%s,' "$(count_pattern "$path" 'arith\.min(si|ui)')"
    printf '%s,' "$(count_pattern "$path" 'd_affine\.min')"
    printf '%s,' "$(tail_guard_count "$path")"
    printf '%s,' "$(count_pattern "$path" 'step %[A-Za-z0-9_]+ : index')"
    printf '%s,' "$(count_pattern "$path" 'step [0-9]+([[:space:]]|:|$)')"
    printf '%s,' "$(count_pattern "$path" 'd_tensor\.nat\.mul')"
    printf '%s,' "$(count_pattern "$path" 'd_tensor\.shape\.to_index')"
    printf '%s,' "$(count_pattern "$path" 'd_affine\.for')"
    printf '%s,' "$(count_pattern "$path" '(^|[^A-Za-z0-9_])affine\.for')"
    printf '%s,' "$ops"
    printf '%s,' "$(line_count "$path")"
    printf '%s,' "$delta"
    printf '%s\n' "$notes"
  } >> "$METRICS"
}

run_scair_pipeline() {
  local input="$1"
  local output="$2"
  local passes="$3"
  "$SCAIR_OPT" "$input" --allow-unregistered-dialect -p "$passes" > "$output"
}

run_mlir_pipeline() {
  local input="$1"
  local output="$2"
  shift 2
  "$MLIR_OPT" "$input" --allow-unregistered-dialect "$@" > "$output"
}

write_route_manifest() {
  local path="$1"
  local json_path="${path%.md}.json"
  cat > "$path" <<'MD'
# Tail/Min Simplifier Route Manifest

| Route | Role |
|---|---|
| `stock_affine_guarded_tile` | Stock affine negative control: upstream affine cleanup keeps the `affine.for ... to min` tail guard for an ordinary product. |
| `ordinary_d_affine_guarded_tile` | Congruent ordinary `d_affine` dynamic-step control: a known-positive RHS permits guarded tiling, but there is no dependent product fact to consume, so the `arith.minsi` tail guard remains. |
| `dependent_guarded_tile_simplified` | Congruent dependent `d_affine` route: explicit `d_tensor.size.mul` facts let `dependent-tail-min-simplify` remove the `arith.minsi` tail guard. |

The simplifier is conservative. Missing a non-standard tail form is an
optimization miss; removing a min without a dependent product proof is invalid.
MD

  cat > "$json_path" <<'JSON'
[
  {
    "route": "stock_affine_guarded_tile",
    "claim_role": "upstream_negative_control",
    "expected_tail": "affine_min",
    "product_representation": "arith.muli"
  },
  {
    "route": "ordinary_d_affine_guarded_tile",
    "claim_role": "ordinary_d_affine_positive_step_no_product_proof_control",
    "expected_tail": "arith_minsi",
    "product_representation": "arith.muli"
  },
  {
    "route": "dependent_guarded_tile_simplified",
    "claim_role": "proof_consuming_tail_cleanup",
    "expected_tail": "none",
    "product_representation": "d_tensor.size.mul"
  }
]
JSON
}

require_file "$DEPENDENT_SOURCE"
require_file "$ORDINARY_SOURCE"
require_file "$STOCK_AFFINE_SOURCE"
require_exe "$SCAIR_OPT"

write_route_manifest "$OUT_DIR/route_manifest.md"

cat > "$METRICS" <<'CSV'
variant,stage,toolchain,status,artifact,affine_min_count,arith_minsi_count,d_affine_min_count,tail_guard_count,dynamic_step_count,static_step_count,d_tensor_nat_mul_count,d_tensor_shape_to_index_count,d_affine_for_count,affine_for_count,total_op_count,mlir_loc,removed_op_delta,notes
CSV

dependent_simplified="$OUT_DIR/dependent_guarded_tile_tail_min_simplified.mlir"
ordinary_cleanup="$OUT_DIR/ordinary_d_affine_guarded_tile_cleanup.mlir"
run_scair_pipeline "$ORDINARY_SOURCE" "$ordinary_cleanup" "ordinary-product-tile-with-tail,canonicalize,cse,dce"
run_scair_pipeline "$DEPENDENT_SOURCE" "$dependent_simplified" "canonicalize-d-tensor-size-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce"

if [[ -x "$MLIR_OPT" ]]; then
  stock_guarded="$(mktemp "$OUT_DIR/stock_affine_guarded_tile.XXXXXX.tmp.mlir")"
  stock_cleanup="$OUT_DIR/stock_affine_guarded_tile_upstream_cleanup.mlir"
  run_scair_pipeline "$STOCK_AFFINE_SOURCE" "$stock_guarded" "ordinary-affine-product-tile-with-tail:4"
  run_mlir_pipeline "$stock_guarded" "$stock_cleanup" --pass-pipeline='builtin.module(func.func(affine-simplify-min-max),canonicalize,cse,symbol-dce)'
  rm -f "$stock_guarded"
  metric_row "stock_affine_guarded_tile" "after_upstream_canonicalize_cse_affine_simplify_minmax" "upstream_mlir" "ok" "$stock_cleanup" "NA" "stock_affine_to_min_retained_after_upstream_cleanup"
else
  skipped="$OUT_DIR/stock_affine_guarded_tile_upstream_cleanup.skipped.mlir"
  cp "$STOCK_AFFINE_SOURCE" "$skipped"
  metric_row "stock_affine_guarded_tile" "after_upstream_canonicalize_cse_affine_simplify_minmax" "upstream_mlir" "skipped" "$skipped" "NA" "mlir-opt_not_found"
fi

metric_row "ordinary_d_affine_guarded_tile" "after_cleanup_no_simplifier" "scair" "ok" "$ordinary_cleanup" "NA" "ordinary_arith_muli_product_tail_min_retained_without_dependent_product_proof"
metric_row "dependent_guarded_tile_simplified" "after_tail_min_simplify_cleanup" "scair" "ok" "$dependent_simplified" "NA" "d_tensor_nat_mul_rhs_proves_tile_end_within_full_bound"

{
  echo "# Tail/Min Simplifier Benchmark Summary"
  echo
  echo "Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo
  echo "| Variant | Stage | affine.min | arith.minsi | tail guards | dynamic steps | static steps | size.mul | size witness erasure | total ops | LOC | Removed delta | Notes |"
  echo "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
  tail -n +2 "$METRICS" | while IFS=, read -r variant stage toolchain status artifact affine_min arith_min d_affine_min tail_guard dynamic_step static_step nat_mul shape_to_index d_affine_for affine_for total_ops loc delta notes; do
    echo "| \`$variant\` | \`$stage\` | $affine_min | $arith_min | $tail_guard | $dynamic_step | $static_step | $nat_mul | $shape_to_index | $total_ops | $loc | $delta | $notes |"
  done
  echo
  echo "Key comparison:"
  echo "- \`stock_affine_guarded_tile\` is the stock affine negative control: upstream \`affine-simplify-min-max\` keeps the \`affine.for ... to min\` tail because it cannot recover the dependent divisibility fact from ordinary SSA arithmetic."
  echo "- \`ordinary_d_affine_guarded_tile\` is the congruent ordinary dynamic-step control: it uses ordinary \`arith.muli\` provenance, emits a tail/min guard, and retains that guard because there is no dependent product proof to consume."
  echo "- \`dependent_guarded_tile_simplified\` is the congruent dependent route: it consumes \`d_tensor.size.mul\` provenance and rewrites the min upper bound to \`tile + tileSize\`, leaving no tail/min guard."
} > "$SUMMARY"

echo "Tail/min simplifier benchmark complete."
echo "Produced:"
echo "  $METRICS"
echo "  $SUMMARY"
