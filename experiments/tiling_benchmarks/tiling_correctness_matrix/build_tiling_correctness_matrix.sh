#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$ROOT/experiments/tiling_benchmarks/tiling_correctness_matrix"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
SCAIR_OPT="${SCAIR_OPT:-$ROOT/out/tools/opt/launcher.dest/run}"

source "$ROOT/experiments/common_metrics.sh"

mkdir -p "$OUT_DIR"

require_bin "$SCAIR_OPT"

run_scair_pipeline() {
  local input="$1"
  local output="$2"
  local passes="$3"
  "$SCAIR_OPT" "$input" --allow-unregistered-dialect -p "$passes" > "$output"
}

require_ir_pattern() {
  local path="$1"
  local pattern="$2"
  local message="$3"
  if ! rg -q "$pattern" "$path"; then
    echo "error: $message in $path" >&2
    exit 1
  fi
}

reject_ir_pattern() {
  local path="$1"
  local pattern="$2"
  local message="$3"
  if rg -q "$pattern" "$path"; then
    echo "error: $message in $path" >&2
    exit 1
  fi
}

tail_bound_kind() {
  local path="$1"
  if rg -q 'arith\.minsi' "$path"; then
    echo "arith_minsi"
  elif rg -q ' to min |affine\.min|d_affine\.min' "$path"; then
    echo "affine_min"
  else
    echo "none"
  fi
}

write_route_manifest() {
  cat > "$OUT_DIR/route_manifest.md" <<'MD'
# Tiling Correctness Matrix Route Manifest

| Case | Canonical route | Expected property |
|---|---|---|
| `ordinary_tail` | `ordinary_tail` | Ordinary `arith.muli` product keeps affine min tail. |
| `non_divisible_ordinary` | `ordinary_tail` | Ordinary static product that is not divisible by the benchmark tile keeps affine min tail. |
| `dependent_exact_dynamic` | `dependent_exact_dynamic` | Explicit `arith.muli` with `index` factor exact-tiles with dynamic step and no tail. |
| `dependent_static_affine` | `dependent_exact_static_affine` | Static index factor exact-tiles and bridges to stock `affine.for` with static step. |
| `runtime_checked_dynamic` | `dependent_exact_runtime_checked` | `cf.assert` refinement exercises runtime-checked index control flow, then lowers to aborting LLVM-style CFG. |
| `zero_negative` | `zero_negative_control` | Explicit index constant 0 blocks exact tiling. |
| `nested_commuted_product` | `dependent_exact_dynamic` | Nested/commuted explicit product exact-tiles by the rightmost positive factor. |
| `nested_commuted_product_lazy` | `dependent_exact_dynamic_lazy_facts` | Nested/commuted exact tiling works without eager product canonicalization. |
| `tail_product_factor_lazy` | `dependent_tail_simplify_lazy_facts` | Tail simplification removes a clamp when the tile size is itself an explicit product factor. |

The matrix is structural. It is meant to defend compiler claims and benchmark
route assumptions; it is not a runtime performance benchmark.
MD

  cat > "$OUT_DIR/route_manifest.json" <<'JSON'
[
  {
    "case": "ordinary_tail",
    "canonical_route": "ordinary_tail",
    "script_route": "ordinary-affine-product-loop-tile-with-tail:3",
    "expected_tail": "affine_min",
    "positivity_source": "not_required_static_tile_control"
  },
  {
    "case": "non_divisible_ordinary",
    "canonical_route": "ordinary_tail",
    "script_route": "ordinary-affine-product-loop-tile-with-tail:3",
    "expected_tail": "affine_min",
    "positivity_source": "not_required_static_tile_control"
  },
  {
    "case": "dependent_exact_dynamic",
    "canonical_route": "dependent_exact_dynamic",
    "script_route": "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps",
    "expected_tail": "none",
    "positivity_source": "index_positive_assertion"
  },
  {
    "case": "dependent_static_affine",
    "canonical_route": "dependent_exact_static_affine",
    "script_route": "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,d-affine-to-affine-compatible",
    "expected_tail": "none",
    "positivity_source": "positive_index_constant"
  },
  {
    "case": "runtime_checked_dynamic",
    "canonical_route": "dependent_exact_runtime_checked",
    "script_route": "canonicalize-d-tensor-shape-products,dependent-exact-tile,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,lower-refined-control-flow-to-llvm,lower-cf-assert-to-llvm",
    "expected_tail": "none",
    "positivity_source": "cf_assert_refinement"
  },
  {
    "case": "zero_negative",
    "canonical_route": "zero_negative_control",
    "script_route": "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile",
    "expected_tail": "not_tiled",
    "positivity_source": "explicit_zero_rejected"
  },
  {
    "case": "nested_commuted_product",
    "canonical_route": "dependent_exact_dynamic",
    "script_route": "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile",
    "expected_tail": "none",
    "positivity_source": "positive_index_constant"
  },
  {
    "case": "nested_commuted_product_lazy",
    "canonical_route": "dependent_exact_dynamic_lazy_facts",
    "script_route": "dependent-product-loop-exact-tile",
    "expected_tail": "none",
    "positivity_source": "positive_index_constant"
  },
  {
    "case": "tail_product_factor_lazy",
    "canonical_route": "dependent_tail_simplify_lazy_facts",
    "script_route": "dependent-tail-min-simplify",
    "expected_tail": "none",
    "positivity_source": "index_product_positive_assertion"
  }
]
JSON
}

append_case_row() {
  local csv="$1"
  local case_name="$2"
  local canonical_route="$3"
  local status="$4"
  local input="$5"
  local tiled="$6"
  local pipeline="$7"
  local case_kind="$8"
  local product_shape="$9"
  local positivity_source="${10}"
  local expected_tail="${11}"
  local notes="${12}"
  local safe_pipeline="${pipeline//,/+}"
  local total_ops
  total_ops="$(count_ops_structural "$tiled")"
  if [[ "$total_ops" == "NA" ]]; then
    total_ops="$(count_ops "$tiled")"
  fi

  {
    printf '%s,%s,%s,%s,%s,%s,' "$case_name" "$canonical_route" "$status" "$(basename "$input")" "$(basename "$tiled")" "$safe_pipeline"
    printf '%s,%s,%s,%s,' "$case_kind" "$product_shape" "$positivity_source" "$expected_tail"
    printf '%s,' "$(tail_bound_kind "$tiled")"
    printf '%s,' "$(count_dynamic_step_ops "$tiled")"
    printf '%s,' "$(count_static_step_ops "$tiled")"
    printf '%s,' "$(count_shape_index_arith_ops "$tiled")"
    printf '%s,' "$(count_d_affine_for_ops "$tiled")"
    printf '%s,' "$(count_affine_for_ops "$tiled")"
    printf '%s,' "$(count_min_ops "$tiled")"
    printf '%s,' "$(count_cf_assert_ops "$tiled")"
    printf '%s,' "$(count_llvm_cond_br_ops "$tiled")"
    printf '%s,' "$(count_abort_calls "$tiled")"
    printf '%s,' "$total_ops"
    printf '%s,' "$(file_metric lines "$tiled")"
    printf '%s\n' "$notes"
  } >> "$csv"
}

validate_case() {
  local case_name="$1"
  local path="$2"
  case "$case_name" in
    ordinary_tail)
      require_ir_pattern "$path" 'arith\.muli' "ordinary route must keep operational index product"
      require_ir_pattern "$path" ' to min ' "ordinary route must keep affine min tail"
      reject_ir_pattern "$path" 'd_affine\.for' "ordinary route must lower to stock affine and keep the ordinary product"
      ;;
    non_divisible_ordinary)
      require_ir_pattern "$path" 'arith\.muli' "non-divisible ordinary route must keep operational index product"
      require_ir_pattern "$path" ' to min ' "non-divisible ordinary route must keep affine min tail"
      reject_ir_pattern "$path" 'd_affine\.for' "non-divisible ordinary route must lower to stock affine and keep the ordinary product"
      ;;
    dependent_exact_dynamic)
      require_ir_pattern "$path" 'arith\.muli' "dependent dynamic route must preserve shape-product proof before erasure"
      reject_ir_pattern "$path" 'arith\.minsi| to min |affine\.min|d_affine\.min' "dependent exact route must not keep tail/min"
      ;;
    dependent_static_affine)
      require_ir_pattern "$path" 'affine\.for %[A-Za-z0-9_]+ = .* step 3' "static route must bridge to stock affine static step"
      reject_ir_pattern "$path" 'd_affine\.for|step %[A-Za-z0-9_]+|arith\.minsi| to min |affine\.min|d_affine\.min' "static route must not keep dependent loops, dynamic steps, or tail/min"
      ;;
    runtime_checked_dynamic)
      require_ir_pattern "$path" 'llvm\.cond_br' "runtime checked route must lower assertion/control flow"
      require_ir_pattern "$path" 'llvm\.call @abort|callee = @abort' "runtime checked route must lower failing assert to abort"
      require_ir_pattern "$path" 'llvm\.unreachable' "runtime checked route must terminate assert failure block"
      reject_ir_pattern "$path" 'd_tensor\.|d_affine\.for|cf\.assert|arith\.minsi| to min |affine\.min|d_affine\.min' "runtime checked route must erase proofs and tails"
      ;;
    zero_negative)
      require_ir_pattern "$path" 'arith\.constant.*value = 0 : index' "zero negative control must keep explicit zero factor"
      require_ir_pattern "$path" 'd_affine\.for %[A-Za-z0-9_]+ = .* step 1 : index' "zero factor must not exact-tile"
      reject_ir_pattern "$path" 'scair\.dependent_product_loop_exact_tile|step 4 : i32|step %[A-Za-z0-9_]+' "zero factor must not produce exact tile loop"
      ;;
    nested_commuted_product)
      require_ir_pattern "$path" 'arith\.muli' "nested/commuted route must preserve explicit product proof before erasure"
      require_ir_pattern "$path" 'd_affine\.for %[A-Za-z0-9_]+ = .* step 7 : i32' "nested/commuted route must tile by explicit rightmost positive factor"
      reject_ir_pattern "$path" 'arith\.minsi| to min |affine\.min|d_affine\.min' "nested/commuted exact route must not keep tail/min"
      ;;
    nested_commuted_product_lazy)
      require_ir_pattern "$path" 'arith\.muli' "lazy nested/commuted route must preserve explicit product proof before erasure"
      require_ir_pattern "$path" 'd_affine\.for %[A-Za-z0-9_]+ = .* step 7 : i32' "lazy nested/commuted route must tile without eager product canonicalization"
      reject_ir_pattern "$path" 'arith\.minsi| to min |affine\.min|d_affine\.min' "lazy nested/commuted exact route must not keep tail/min"
      ;;
    tail_product_factor_lazy)
      require_ir_pattern "$path" 'arith\.muli' "tail product-factor route must preserve explicit product proof before erasure"
      require_ir_pattern "$path" 'step %[A-Za-z0-9_]+ : index' "tail product-factor route must retain a proven dynamic step"
      require_ir_pattern "$path" 'arith\.minsi' "tail product-factor route keeps the clamp in the current index-only subset"
      ;;
  esac
}

run_case() {
  local case_name="$1"
  local canonical_route="$2"
  local pipeline="$3"
  local case_kind="$4"
  local product_shape="$5"
  local positivity_source="$6"
  local expected_tail="$7"
  local notes="$8"
  local input="$EXAMPLE_DIR/$case_name.mlir"
  local output="$OUT_DIR/$case_name.tiled.mlir"

  require_file "$input"
  cp "$input" "$OUT_DIR/$case_name.input.mlir"
  run_scair_pipeline "$input" "$output" "$pipeline"
  validate_case "$case_name" "$output"
  append_case_row "$METRICS" "$case_name" "$canonical_route" "ok" "$input" "$output" "$pipeline" "$case_kind" "$product_shape" "$positivity_source" "$expected_tail" "$notes"
}

METRICS="$OUT_DIR/metrics.csv"
SUMMARY="$OUT_DIR/summary.md"
METRICS_JSON="$OUT_DIR/metrics.json"

write_route_manifest

cat > "$METRICS" <<'CSV'
case,canonical_route,status,input,tiled,pipeline,case_kind,product_shape,positivity_source,expected_tail,tail_bound_kind,dynamic_step_count,static_step_count,shape_index_arith_op_count,d_affine_for_count,affine_for_count,min_op_count,cf_assert_count,llvm_cond_br_count,abort_call_count,total_ops,mlir_loc,notes
CSV

run_case "ordinary_tail" \
  "ordinary_tail" \
  "ordinary-affine-product-loop-tile-with-tail:3" \
  "negative_control" \
  "arith.muli" \
  "not_required_static_tile_control" \
  "affine_min" \
  "ordinary_runtime_product_keeps_tail"

run_case "non_divisible_ordinary" \
  "ordinary_tail" \
  "ordinary-affine-product-loop-tile-with-tail:3" \
  "negative_control" \
  "const5*const2_tile3" \
  "not_required_static_tile_control" \
  "affine_min" \
  "ordinary_static_product_not_divisible_by_tile_keeps_tail"

run_case "dependent_exact_dynamic" \
  "dependent_exact_dynamic" \
  "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps" \
  "positive_exact" \
  "K0*K1" \
  "index_positive_assertion" \
  "none" \
  "explicit_shape_product_index_factor_removes_tail"

run_case "dependent_static_affine" \
  "dependent_exact_static_affine" \
  "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,d-affine-to-affine-compatible,canonicalize,cse,dce" \
  "positive_static_bridge" \
  "K0*const3" \
  "positive_index_constant" \
  "none" \
  "static_index_const_factor_bridges_to_stock_affine"

run_case "runtime_checked_dynamic" \
  "dependent_exact_runtime_checked" \
  "canonicalize-d-tensor-shape-products,dependent-exact-tile,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,lower-refined-control-flow-to-llvm,lower-cf-assert-to-llvm,canonicalize,cse,dce" \
  "positive_runtime_checked" \
  "K0*K1" \
  "cf_assert_refinement" \
  "none" \
  "runtime_assert_refines_dynamic_factor_then_lowers_to_abort_cfg"

run_case "zero_negative" \
  "zero_negative_control" \
  "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile" \
  "negative_control" \
  "const4*const0" \
  "explicit_zero_rejected" \
  "not_tiled" \
  "explicit_index_const_zero_blocks_exact_tiling"

run_case "nested_commuted_product" \
  "dependent_exact_dynamic" \
  "canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile" \
  "positive_exact" \
  "(K1*K0)*K2" \
  "positive_index_constant" \
  "none" \
  "nested_commuted_explicit_product_tiles_by_rightmost_positive_factor"

run_case "nested_commuted_product_lazy" \
  "dependent_exact_dynamic_lazy_facts" \
  "dependent-product-loop-exact-tile" \
  "positive_exact_lazy_facts" \
  "(K1*K0)*K2" \
  "positive_index_constant" \
  "none" \
  "same_nested_commuted_case_tiles_without_eager_product_canonicalization"

run_case "tail_product_factor_lazy" \
  "dependent_tail_simplify_lazy_facts" \
  "dependent-tail-min-simplify" \
  "negative_tail_cleanup_lazy_facts" \
  "(K1*K0)*K2 contains (K1*K0)" \
  "index_product_positive_assertion" \
  "arith_minsi" \
  "tail_clamp_retained_without_nat_factor_subset_proof_or_symbolic_solver"

{
  echo "# Tiling Correctness Matrix"
  echo
  echo "| Case | Route | Status | Product | Positivity | Expected tail | Observed tail | Dynamic steps | Static steps | Index arith ops | Min ops | Notes |"
  echo "|---|---|---|---|---|---|---|---:|---:|---:|---:|---|"
  tail -n +2 "$METRICS" | while IFS=, read -r case_name canonical_route status input tiled pipeline case_kind product_shape positivity_source expected_tail observed_tail dynamic_steps static_steps index_arith_ops d_affine_for affine_for min_ops cf_assert llvm_cond_br abort_calls total_ops loc notes; do
    echo "| \`$case_name\` | \`$canonical_route\` | $status | \`$product_shape\` | \`$positivity_source\` | \`$expected_tail\` | \`$observed_tail\` | $dynamic_steps | $static_steps | $index_arith_ops | $min_ops | $notes |"
  done
} > "$SUMMARY"

python3 - "$METRICS" "$METRICS_JSON" <<'PY'
import csv
import json
import sys
from pathlib import Path

csv_path = Path(sys.argv[1])
json_path = Path(sys.argv[2])
rows = []
for row in csv.DictReader(csv_path.open(newline="", encoding="utf-8")):
    rows.append(row)
json_path.write_text(json.dumps(rows, indent=2, sort_keys=True) + "\n", encoding="utf-8")
PY

echo "Tiling correctness matrix complete."
echo "Produced:"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/route_manifest.md"
echo "  $OUT_DIR/route_manifest.json"
