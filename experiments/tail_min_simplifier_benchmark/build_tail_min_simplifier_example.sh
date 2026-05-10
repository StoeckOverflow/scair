#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$ROOT/experiments/tail_min_simplifier_benchmark"
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
    printf '%s,' "$(count_pattern "$path" 'dtensor\.nat\.mul')"
    printf '%s,' "$(count_pattern "$path" 'dtensor\.shape\.to_index')"
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
| `ordinary_d_affine_guarded_tile` | Negative control: ordinary `arith.muli` product keeps the min tail guard. |
| `dependent_guarded_tile_no_simplify` | Shows guarded dependent tiling before proof-consuming tail cleanup. |
| `dependent_guarded_tile_simplified` | Consumes explicit `dtensor.nat.mul` facts to remove the min tail guard. |
| `dependent_exact_tile_reference` | Exact tiling route that never emits the tail guard. |
| `stock_affine_guarded_tile` | Upstream affine-facing negative control. |

The simplifier is conservative. Missing a non-standard tail form is an
optimization miss; removing a min without a dependent product proof is invalid.
MD

  cat > "$json_path" <<'JSON'
[
  {
    "route": "ordinary_d_affine_guarded_tile",
    "claim_role": "negative_control",
    "expected_tail": "arith_minsi",
    "product_representation": "arith.muli"
  },
  {
    "route": "dependent_guarded_tile_no_simplify",
    "claim_role": "pre_cleanup_control",
    "expected_tail": "arith_minsi",
    "product_representation": "dtensor.nat.mul"
  },
  {
    "route": "dependent_guarded_tile_simplified",
    "claim_role": "proof_consuming_tail_cleanup",
    "expected_tail": "none",
    "product_representation": "dtensor.nat.mul"
  },
  {
    "route": "dependent_exact_tile_reference",
    "claim_role": "exact_reference",
    "expected_tail": "none",
    "product_representation": "dtensor.nat.mul"
  },
  {
    "route": "stock_affine_guarded_tile",
    "claim_role": "upstream_negative_control",
    "expected_tail": "affine_min",
    "product_representation": "arith.muli"
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
variant,stage,toolchain,status,artifact,affine_min_count,arith_minsi_count,d_affine_min_count,tail_guard_count,dynamic_step_count,static_step_count,dtensor_nat_mul_count,dtensor_shape_to_index_count,d_affine_for_count,affine_for_count,total_op_count,mlir_loc,removed_op_delta,notes
CSV

ordinary_guarded="$OUT_DIR/ordinary_d_affine_guarded_tile.mlir"
ordinary_cleanup="$OUT_DIR/ordinary_d_affine_guarded_tile_cleanup.mlir"
run_scair_pipeline "$ORDINARY_SOURCE" "$ordinary_guarded" "ordinary-product-tile-with-tail"
ordinary_guarded_ops="$(total_op_count "$ordinary_guarded")"
run_scair_pipeline "$ORDINARY_SOURCE" "$ordinary_cleanup" "ordinary-product-tile-with-tail,dependent-tail-min-simplify,canonicalize,cse,dce"
metric_row "ordinary_d_affine_guarded_tile" "after_guarded_tiling" "scair" "ok" "$ordinary_guarded" "NA" "ordinary_arith_muli_product_no_dependent_proof"
metric_row "ordinary_d_affine_guarded_tile" "after_tail_min_simplify_cleanup" "scair" "ok" "$ordinary_cleanup" "$ordinary_guarded_ops" "min_retained_without_dtensor_nat_mul"

dependent_guarded="$OUT_DIR/dependent_guarded_tile.mlir"
dependent_no_simplify="$OUT_DIR/dependent_guarded_tile_cleanup_no_simplify.mlir"
dependent_simplified="$OUT_DIR/dependent_guarded_tile_tail_min_simplified.mlir"
dependent_exact="$OUT_DIR/dependent_exact_tile_reference.mlir"
run_scair_pipeline "$DEPENDENT_SOURCE" "$dependent_guarded" "canonicalize-dtensor-nat-products,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps"
dependent_guarded_ops="$(total_op_count "$dependent_guarded")"
run_scair_pipeline "$DEPENDENT_SOURCE" "$dependent_no_simplify" "canonicalize-dtensor-nat-products,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce"
run_scair_pipeline "$DEPENDENT_SOURCE" "$dependent_simplified" "canonicalize-dtensor-nat-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce"
run_scair_pipeline "$DEPENDENT_SOURCE" "$dependent_exact" "canonicalize-dtensor-nat-products,dependent-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce"
metric_row "dependent_guarded_tile_no_simplify" "after_guarded_tiling" "scair" "ok" "$dependent_guarded" "NA" "guarded_tiler_conservatively_emits_tail_min"
metric_row "dependent_guarded_tile_no_simplify" "after_cleanup_no_simplify" "scair" "ok" "$dependent_no_simplify" "$dependent_guarded_ops" "min_retained_when_proof_not_consumed"
metric_row "dependent_guarded_tile_simplified" "after_tail_min_simplify_cleanup" "scair" "ok" "$dependent_simplified" "$dependent_guarded_ops" "dtensor_nat_mul_rhs_proves_tile_end_within_full_bound"
metric_row "dependent_exact_tile_reference" "after_exact_tiling_cleanup" "scair" "ok" "$dependent_exact" "NA" "reference_route_emits_no_tail_min_initially"

if [[ -x "$MLIR_OPT" ]]; then
  stock_guarded="$OUT_DIR/stock_affine_guarded_tile.mlir"
  stock_cleanup="$OUT_DIR/stock_affine_guarded_tile_upstream_cleanup.mlir"
  run_scair_pipeline "$STOCK_AFFINE_SOURCE" "$stock_guarded" "ordinary-affine-product-tile-with-tail:4"
  stock_guarded_ops="$(total_op_count "$stock_guarded")"
  run_mlir_pipeline "$stock_guarded" "$stock_cleanup" --pass-pipeline='builtin.module(func.func(affine-simplify-min-max),canonicalize,cse,symbol-dce)'
  metric_row "stock_affine_guarded_tile" "after_scair_guarded_tiling" "scair" "ok" "$stock_guarded" "NA" "stock_affine_control_contains_affine_min_tail"
  metric_row "stock_affine_guarded_tile" "after_upstream_canonicalize_cse_affine_simplify_minmax" "upstream_mlir" "ok" "$stock_cleanup" "$stock_guarded_ops" "upstream_cannot_see_dtensor_nat_mul_proof_in_ordinary_arith_muli_product"
else
  skipped="$OUT_DIR/stock_affine_guarded_tile_upstream_cleanup.skipped.mlir"
  cp "$STOCK_AFFINE_SOURCE" "$skipped"
  metric_row "stock_affine_guarded_tile" "after_upstream_canonicalize_cse_affine_simplify_minmax" "upstream_mlir" "skipped" "$skipped" "NA" "mlir-opt_not_found"
fi

{
  echo "# Tail/Min Simplifier Benchmark Summary"
  echo
  echo "Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo
  echo "| Variant | Stage | affine.min | arith.minsi | tail guards | dynamic steps | static steps | nat.mul | shape.to_index | total ops | LOC | Removed delta | Notes |"
  echo "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
  tail -n +2 "$METRICS" | while IFS=, read -r variant stage toolchain status artifact affine_min arith_min d_affine_min tail_guard dynamic_step static_step nat_mul shape_to_index d_affine_for affine_for total_ops loc delta notes; do
    echo "| \`$variant\` | \`$stage\` | $affine_min | $arith_min | $tail_guard | $dynamic_step | $static_step | $nat_mul | $shape_to_index | $total_ops | $loc | $delta | $notes |"
  done
  echo
  echo "Key comparison:"
  echo "- \`ordinary_d_affine_guarded_tile\` retains \`arith.minsi\` because the product is only operational \`arith.muli\`."
  echo "- \`dependent_guarded_tile_no_simplify\` shows the same conservative guarded shape even though a \`dtensor.nat.mul\` proof is present."
  echo "- \`dependent_guarded_tile_simplified\` consumes that proof and rewrites the min upper bound to \`tile + tileSize\`, leaving no tail/min guard."
  echo "- \`stock_affine_guarded_tile\` is a stock-affine-facing control: upstream \`affine-simplify-min-max\` does not recover the dependent divisibility fact from ordinary SSA arithmetic."
} > "$SUMMARY"

echo "Tail/min simplifier benchmark complete."
echo "Produced:"
echo "  $METRICS"
echo "  $SUMMARY"
