#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$ROOT/experiments/conv_tiling_benchmark"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
SCAIR_OPT="${SCAIR_OPT:-$ROOT/out/tools/opt/launcher.dest/run}"
CONV_TILING_ROUTES="${CONV_TILING_ROUTES:-ordinary_conv_tail,dependent_conv_guarded_tail_simplified,dependent_conv_exact_dynamic,dependent_conv_exact_static_affine}"
CONV_TILING_ORDINARY_TILE="${CONV_TILING_ORDINARY_TILE:-5}"

source "$ROOT/experiments/common_metrics.sh"

mkdir -p "$OUT_DIR"

require_bin "$SCAIR_OPT"

route_enabled() {
  local route="$1"
  local entry
  IFS=',' read -r -a CONV_TILING_ROUTE_LIST <<<"$CONV_TILING_ROUTES"
  for entry in "${CONV_TILING_ROUTE_LIST[@]}"; do
    if [[ "$entry" == "$route" || "$entry" == "all" ]]; then
      return 0
    fi
  done
  return 1
}

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
# Conv Tiling Route Manifest

| Case | Canonical route | Expected property |
|---|---|---|
| `ordinary_conv_tail` | `ordinary_conv_tail` | Full Conv2D kernel with ordinary `Ci * Kh * Kw` index product keeps tail/min cleanup. |
| `dependent_conv_guarded_tail_simplified` | `dependent_conv_guarded_tail_simplified` | Full Conv2D kernel uses the same guarded tiling shape, then removes the emitted tail/min for a proven-positive dynamic `Kw` factor with explicit `dtensor.nat.mul` proof. |
| `dependent_conv_exact_dynamic` | `dependent_conv_exact_dynamic` | Full Conv2D kernel with explicit `dtensor.nat.mul` product exact-tiles by the dynamic `Kw` factor with no tail. |
| `dependent_conv_exact_static_affine` | `dependent_conv_exact_static_affine` | Full specialized 3x3 Conv2D kernel exact-tiles by static `Kh * Kw = 9` and bridges to stock `affine.for`. |

This is a structural tiling benchmark over the same Conv2D kernel shape used by
`experiments/convolution_benchmark`: N/Cout/OH/OW outer loops, flat input/filter
reinterpret casts, and the full multiply-accumulate body. The reduction is
flattened to one `Ci * Kh * Kw` loop so the product tiler can act on the
convolution reduction domain directly.
MD

  cat > "$OUT_DIR/route_manifest.json" <<'JSON'
[
  {
    "case": "ordinary_conv_tail",
    "canonical_route": "ordinary_conv_tail",
    "script_route": "ordinary-affine-product-loop-tile-with-tail",
    "expected_tail": "affine_min",
    "product_representation": "arith.muli",
    "positivity_source": "not_required_static_tile_control"
  },
  {
    "case": "dependent_conv_guarded_tail_simplified",
    "canonical_route": "dependent_conv_guarded_tail_simplified",
    "script_route": "canonicalize-dtensor-nat-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps",
    "expected_tail": "none_after_simplification",
    "guarded_stage_tail": "arith_minsi",
    "product_representation": "dtensor.nat.mul",
    "positivity_source": "posnat_kw_type"
  },
  {
    "case": "dependent_conv_exact_dynamic",
    "canonical_route": "dependent_conv_exact_dynamic",
    "script_route": "canonicalize-dtensor-nat-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps",
    "expected_tail": "none",
    "product_representation": "dtensor.nat.mul",
    "positivity_source": "posnat_kw_type"
  },
  {
    "case": "dependent_conv_exact_static_affine",
    "canonical_route": "dependent_conv_exact_static_affine",
    "script_route": "canonicalize-dtensor-nat-products,dependent-product-loop-exact-tile,d-affine-to-affine-compatible",
    "expected_tail": "none",
    "product_representation": "dtensor.nat.mul+nat.const",
    "positivity_source": "nat_const_positive"
  }
]
JSON
}

append_case_row() {
  local case_name="$1"
  local canonical_route="$2"
  local status="$3"
  local input="$4"
  local tiled="$5"
  local pipeline="$6"
  local product_shape="$7"
  local tile_factor="$8"
  local positivity_source="$9"
  local expected_tail="${10}"
  local notes="${11}"
  local safe_pipeline="${pipeline//,/+}"
  local total_ops
  total_ops="$(count_ops_structural "$tiled")"
  if [[ "$total_ops" == "NA" ]]; then
    total_ops="$(count_ops "$tiled")"
  fi

  {
    printf '%s,%s,%s,%s,%s,%s,' "$case_name" "$canonical_route" "$status" "$(basename "$input")" "$(basename "$tiled")" "$safe_pipeline"
    printf '%s,%s,%s,%s,' "$product_shape" "$tile_factor" "$positivity_source" "$expected_tail"
    printf '%s,' "$(tail_bound_kind "$tiled")"
    printf '%s,' "$(count_dynamic_step_ops "$tiled")"
    printf '%s,' "$(count_static_step_ops "$tiled")"
    printf '%s,' "$(count_dtensor_nat_ops "$tiled")"
    printf '%s,' "$(count_d_affine_for_ops "$tiled")"
    printf '%s,' "$(count_affine_for_ops "$tiled")"
    printf '%s,' "$(count_min_ops "$tiled")"
    printf '%s,' "$total_ops"
    printf '%s,' "$(file_metric lines "$tiled")"
    printf '%s\n' "$notes"
  } >> "$METRICS_CSV"
}

validate_case() {
  local case_name="$1"
  local path="$2"
  case "$case_name" in
    ordinary_conv_tail)
      require_ir_pattern "$path" 'arith\.muli' "ordinary conv route must keep operational index product"
      require_ir_pattern "$path" ' to min |affine\.min|arith\.minsi' "ordinary conv route must keep tail/min cleanup"
      require_ir_pattern "$path" 'memref\.load|memref\.store' "ordinary conv route must keep full Conv2D memory body"
      reject_ir_pattern "$path" 'dtensor\.nat\.mul|d_affine\.for' "ordinary conv route must not use dependent product proof or d_affine loop"
      ;;
    dependent_conv_exact_dynamic)
      require_ir_pattern "$path" 'dtensor\.nat\.mul' "dependent conv route must preserve nat product proof before erasure"
      require_ir_pattern "$path" 'step %[A-Za-z0-9_]+ : index' "dependent conv route must use a proven-positive dynamic Kw factor step"
      require_ir_pattern "$path" 'd_memref\.load|d_memref\.store' "dependent conv route must keep full Conv2D d_memref memory body"
      reject_ir_pattern "$path" 'arith\.minsi| to min |affine\.min|d_affine\.min' "dependent conv exact route must not keep tail/min"
      ;;
    dependent_conv_guarded_tail_simplified)
      require_ir_pattern "$path" 'dtensor\.nat\.mul' "dependent guarded conv route must preserve nat product proof before erasure"
      require_ir_pattern "$path" 'step %[A-Za-z0-9_]+ : index' "dependent guarded conv route must use a proven-positive dynamic Kw factor step"
      require_ir_pattern "$path" 'd_memref\.load|d_memref\.store' "dependent guarded conv route must keep full Conv2D d_memref memory body"
      reject_ir_pattern "$path" 'arith\.minsi| to min |affine\.min|d_affine\.min' "dependent guarded conv route must remove tail/min after simplification"
      ;;
    dependent_conv_exact_static_affine)
      require_ir_pattern "$path" 'affine\.for %[A-Za-z0-9_]+ = .* step 9' "static conv route must bridge to stock affine static KhKw step"
      require_ir_pattern "$path" 'memref\.load|memref\.store' "static conv route must keep full Conv2D memory body"
      reject_ir_pattern "$path" 'd_affine\.for|step %[A-Za-z0-9_]+|arith\.minsi| to min |affine\.min|d_affine\.min' "static conv route must not keep dependent loops, dynamic steps, or tail/min"
      ;;
  esac
}

run_case() {
  local case_name="$1"
  local canonical_route="$2"
  local input_name="$3"
  local pipeline="$4"
  local product_shape="$5"
  local tile_factor="$6"
  local positivity_source="$7"
  local expected_tail="$8"
  local notes="$9"
  local input="$EXAMPLE_DIR/$input_name"
  local output="$OUT_DIR/$case_name.tiled.mlir"

  require_file "$input"
  cp "$input" "$OUT_DIR/$case_name.input.mlir"
  run_scair_pipeline "$input" "$output" "$pipeline"
  validate_case "$case_name" "$output"
  append_case_row "$case_name" "$canonical_route" "ok" "$input" "$output" "$pipeline" "$product_shape" "$tile_factor" "$positivity_source" "$expected_tail" "$notes"
}

run_guarded_tail_simplified_case() {
  local case_name="$1"
  local canonical_route="$2"
  local input_name="$3"
  local guarded_pipeline="$4"
  local simplified_pipeline="$5"
  local product_shape="$6"
  local tile_factor="$7"
  local positivity_source="$8"
  local expected_tail="$9"
  local notes="${10}"
  local input="$EXAMPLE_DIR/$input_name"
  local guarded="$OUT_DIR/$case_name.guarded.mlir"
  local output="$OUT_DIR/$case_name.tiled.mlir"
  local observed_guarded_tail

  require_file "$input"
  cp "$input" "$OUT_DIR/$case_name.input.mlir"
  run_scair_pipeline "$input" "$guarded" "$guarded_pipeline"
  require_ir_pattern "$guarded" 'arith\.minsi| to min |affine\.min|d_affine\.min' "guarded dependent conv route must emit a tail/min before simplification"
  observed_guarded_tail="$(tail_bound_kind "$guarded")"

  run_scair_pipeline "$input" "$output" "$simplified_pipeline"
  validate_case "$case_name" "$output"
  append_case_row "$case_name" "$canonical_route" "ok" "$input" "$output" "$simplified_pipeline" "$product_shape" "$tile_factor" "$positivity_source" "$expected_tail" "$notes;guarded_tail_bound_kind=$observed_guarded_tail;guarded_artifact=$(basename "$guarded")"
}

METRICS_CSV="$OUT_DIR/metrics.csv"
METRICS_JSON="$OUT_DIR/metrics.json"
SUMMARY_MD="$OUT_DIR/summary.md"

write_route_manifest

cat > "$METRICS_CSV" <<'CSV'
case,canonical_route,status,input,tiled,pipeline,product_shape,tile_factor,positivity_source,expected_tail,tail_bound_kind,dynamic_step_count,static_step_count,nat_proof_op_count,d_affine_for_count,affine_for_count,min_op_count,total_ops,mlir_loc,notes
CSV

if route_enabled "ordinary_conv_tail"; then
  run_case \
    "ordinary_conv_tail" \
    "ordinary_conv_tail" \
    "ordinary_conv2d_tiling_kernel.mlir" \
    "canonicalize,cse,dce,ordinary-affine-product-loop-tile-with-tail:$CONV_TILING_ORDINARY_TILE" \
    "Ci*Kh*Kw as arith.muli" \
    "static_$CONV_TILING_ORDINARY_TILE" \
    "not_required_static_tile_control" \
    "affine_min" \
    "ordinary_index_full_conv2d_product_keeps_tail"
fi

if route_enabled "dependent_conv_exact_dynamic"; then
  run_case \
    "dependent_conv_exact_dynamic" \
    "dependent_conv_exact_dynamic" \
    "dependent_conv2d_tiling_kernel.mlir" \
    "canonicalize,cse,dce,canonicalize-dtensor-nat-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce" \
    "Ci*(Kh*Kw) as dtensor.nat.mul" \
    "dynamic_Kw" \
    "posnat_kw_type" \
    "none" \
    "explicit_full_conv2d_product_exact_tiles_by_kw_factor_without_tail"
fi

if route_enabled "dependent_conv_guarded_tail_simplified"; then
  run_guarded_tail_simplified_case \
    "dependent_conv_guarded_tail_simplified" \
    "dependent_conv_guarded_tail_simplified" \
    "dependent_conv2d_tiling_kernel.mlir" \
    "canonicalize,cse,dce,canonicalize-dtensor-nat-products,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce" \
    "canonicalize,cse,dce,canonicalize-dtensor-nat-products,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce" \
    "Ci*(Kh*Kw) as dtensor.nat.mul" \
    "dynamic_Kw" \
    "posnat_kw_type" \
    "none" \
    "same_guarded_conv2d_tiling_shape_tail_removed_by_dependent_kw_factor_proof"
fi

if route_enabled "dependent_conv_exact_static_affine"; then
  run_case \
    "dependent_conv_exact_static_affine" \
    "dependent_conv_exact_static_affine" \
    "dependent_conv2d_tiling_kernel_static_3x3.mlir" \
    "canonicalize,cse,dce,canonicalize-dtensor-nat-products,dependent-product-loop-exact-tile,d-affine-to-affine-compatible,canonicalize,cse,dce" \
    "Ci*9 as dtensor.nat.mul for specialized 3x3 Conv2D" \
    "static_9" \
    "nat_const_positive" \
    "none" \
    "static_full_conv2d_filter_area_factor_bridges_to_stock_affine"
fi

{
  echo "# Conv Tiling Benchmark Summary"
  echo
  echo "| Case | Route | Status | Product | Tile factor | Positivity | Expected tail | Observed tail | Dynamic steps | Static steps | Nat proof ops | Min ops | Notes |"
  echo "|---|---|---|---|---|---|---|---|---:|---:|---:|---:|---|"
  tail -n +2 "$METRICS_CSV" | while IFS=, read -r case_name canonical_route status input tiled pipeline product_shape tile_factor positivity_source expected_tail observed_tail dynamic_steps static_steps nat_ops d_affine_for affine_for min_ops total_ops loc notes; do
    echo "| \`$case_name\` | \`$canonical_route\` | $status | \`$product_shape\` | \`$tile_factor\` | \`$positivity_source\` | \`$expected_tail\` | \`$observed_tail\` | $dynamic_steps | $static_steps | $nat_ops | $min_ops | $notes |"
  done
} > "$SUMMARY_MD"

python3 - "$METRICS_CSV" "$METRICS_JSON" <<'PY'
import csv
import json
import sys
from pathlib import Path

csv_path = Path(sys.argv[1])
json_path = Path(sys.argv[2])
rows = list(csv.DictReader(csv_path.open(newline="", encoding="utf-8")))
json_path.write_text(json.dumps(rows, indent=2, sort_keys=True) + "\n", encoding="utf-8")
PY

echo "Conv tiling benchmark complete."
echo "Produced:"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
echo "  $OUT_DIR/summary.md"
echo "  $OUT_DIR/route_manifest.md"
echo "  $OUT_DIR/route_manifest.json"
