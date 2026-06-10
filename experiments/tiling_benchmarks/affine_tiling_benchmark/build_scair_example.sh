#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-clean-build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"
AFFINE_TILING_SIZE_SET="$(limit_csv_entries "$AFFINE_TILING_SIZE_SET")"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="${MLIR_OPT:-$BIN_DIR/mlir-opt}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"

AFFINE_TILING_SIZE_SET="${AFFINE_TILING_SIZE_SET:-16x3}"
AFFINE_TILING_ROUTES="${AFFINE_TILING_ROUTES:-mlir_runtime_product,mlir_static_factor_reference,ordinary_scair_product_with_tail,value_dependent_exact_product,value_dependent_static_affine_compatible}"

MLIR_RUNTIME_PRODUCT_SRC="${MLIR_RUNTIME_PRODUCT_SRC:-$EXAMPLE_DIR/affine_runtime_product_mlir.mlir}"
MLIR_STATIC_FACTOR_SRC="${MLIR_STATIC_FACTOR_SRC:-$EXAMPLE_DIR/affine_static_factor_mlir.mlir}"
ORDINARY_PRODUCT_SRC="${ORDINARY_PRODUCT_SRC:-$EXAMPLE_DIR/affine_runtime_product_scair_ordinary.mlir}"
VALUE_DEP_PRODUCT_SRC="${VALUE_DEP_PRODUCT_SRC:-$EXAMPLE_DIR/affine_shape_product_scair_value_dependent.mlir}"
VALUE_DEP_STATIC_SRC="${VALUE_DEP_STATIC_SRC:-$EXAMPLE_DIR/affine_shape_product_scair_value_dependent_static.mlir}"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
BENCHMARK_NAME="affine_tiling"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_file "$MLIR_RUNTIME_PRODUCT_SRC"
require_file "$MLIR_STATIC_FACTOR_SRC"
require_file "$ORDINARY_PRODUCT_SRC"
require_file "$VALUE_DEP_PRODUCT_SRC"
require_file "$VALUE_DEP_STATIC_SRC"

route_enabled() {
  local route="$1"
  local entry
  IFS=',' read -r -a AFFINE_TILING_ROUTE_LIST <<<"$AFFINE_TILING_ROUTES"
  for entry in "${AFFINE_TILING_ROUTE_LIST[@]}"; do
    if [[ "$entry" == "$route" || "$entry" == "all" ]]; then
      return 0
    fi
    case "$route:$entry" in
      mlir_runtime_product:stock_affine_baseline|\
      mlir_static_factor_reference:static_affine_reference|\
      ordinary_scair_product_with_tail:ordinary_tail|\
      ordinary_scair_product_with_tail:ordinary_tail_preserving|\
      value_dependent_exact_product:dependent_exact_dynamic|\
      value_dependent_exact_product:dependent_exact_runtime_checked|\
      value_dependent_static_affine_compatible:dependent_exact_static_affine)
        return 0
        ;;
    esac
  done
  return 1
}

write_route_manifest() {
  local path="$1"
  local json_path="${path%.md}.json"
  cat > "$path" <<'MD'
# Affine Tiling Route Manifest

| Canonical route | Script route | Role |
|---|---|---|
| `stock_affine_baseline` | `mlir_runtime_product` | Ordinary runtime product tiled by upstream affine; keeps min/tail. |
| `static_affine_reference` | `mlir_static_factor_reference` | Static stock-affine factor reference with no min/tail. |
| `ordinary_tail` | `ordinary_scair_product_with_tail` | ScaIR ordinary index-product tiling; keeps min/tail. |
| `dependent_exact_dynamic` | `value_dependent_exact_product` | Dependent exact tiling from explicit `arith.muli` product proof with a runtime-checked positive index factor. |
| `dependent_exact_static_affine` | `value_dependent_static_affine_compatible` | Dependent exact tiling bridged to stock `affine.for`. |

Important ScaIR pipelines:

```text
ordinary_tail:
  canonicalize,cse,dce,
  ordinary-affine-product-loop-tile-with-tail:<k1>

dependent_exact_dynamic:
  canonicalize,cse,dce,
  canonicalize-d-tensor-shape-products,
  dependent-product-loop-exact-tile,
  validate-d-affine-dynamic-steps

dependent_exact_static_affine:
  canonicalize,cse,dce,
  canonicalize-d-tensor-shape-products,
  dependent-product-loop-exact-tile,
  d-affine-to-affine-compatible,
  canonicalize,cse,dce
```

Exact routes require explicit shape-rooted index product provenance. Ordinary `arith.muli`
routes are negative controls and should retain tail guards.
MD

  cat > "$json_path" <<'JSON'
[
  {
    "canonical_route": "stock_affine_baseline",
    "script_route": "mlir_runtime_product",
    "claim_role": "ordinary_runtime_product",
    "expected_tail": "affine_min",
    "product_representation": "arith.muli"
  },
  {
    "canonical_route": "static_affine_reference",
    "script_route": "mlir_static_factor_reference",
    "claim_role": "static_affine_reference",
    "expected_tail": "none",
    "product_representation": "affine_static_factor"
  },
  {
    "canonical_route": "ordinary_tail",
    "script_route": "ordinary_scair_product_with_tail",
    "claim_role": "ordinary_runtime_product",
    "expected_tail": "affine_min",
    "product_representation": "arith.muli"
  },
  {
    "canonical_route": "dependent_exact_dynamic",
    "script_route": "value_dependent_exact_product",
    "claim_role": "value_dependent_exact",
    "expected_tail": "none",
    "product_representation": "arith.muli",
    "positivity_source": "index_positive_assertion"
  },
  {
    "canonical_route": "dependent_exact_static_affine",
    "script_route": "value_dependent_static_affine_compatible",
    "claim_role": "value_dependent_static_affine_compatible",
    "expected_tail": "none",
    "product_representation": "arith.muli",
    "positivity_source": "positive_index_constant"
  }
]
JSON
}

run_scair_opt() {
  "$SCAIR_OPT" "$@" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)'
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

first_index_product_value() {
  local path="$1"
  sed -nE 's/^[[:space:]]*(%[A-Za-z0-9_]+) = "?arith\.muli"?.* : (\(index, index\) -> index|index)$/\1/p' "$path" | head -n 1
}

tail_bound_kind() {
  local path="$1"
  if rg -q 'arith\.minsi' "$path"; then
    echo "arith_minsi"
  elif rg -q ' to min |affine\.min|d_affine\.min' "$path"; then
    echo "to_min"
  else
    echo "none"
  fi
}

validate_runtime_product_with_min() {
  local path="$1"
  local tile_size="$2"
  local product
  product="$(first_index_product_value "$path")"
  if [[ -z "$product" ]]; then
    echo "error: runtime product route must compute upper bound with arith.muli in $path" >&2
    exit 1
  fi
  require_ir_pattern "$path" "affine\\.for %[A-Za-z0-9_]+ = (0|#[A-Za-z0-9_]+\\(%[A-Za-z0-9_]+\\)) to (#[A-Za-z0-9_]+\\(\\)\\[$product\\]|$product) step $tile_size" \
    "runtime product route must tile the arith.muli product bound with the requested static step"
  require_ir_pattern "$path" " to min " \
    "runtime product route must keep a min tail bound"
  reject_ir_pattern "$path" 'd_tensor[.]shape[.]to_index|d_affine\.for' \
    "runtime product route must stay ordinary MLIR/Affine"
}

validate_static_factor_no_min() {
  local path="$1"
  local tile_size="$2"
  require_ir_pattern "$path" "step $tile_size" \
    "static affine factor reference must tile with the requested static step"
  reject_ir_pattern "$path" ' to min |affine\.min|d_affine\.min|arith\.minsi|remainder| mod|cleanup' \
    "static affine factor reference must not need min/tail cleanup"
  reject_ir_pattern "$path" 'arith\.muli|arith\.muli|d_affine\.for' \
    "static affine factor reference must isolate stock affine static factor reasoning"
}

validate_ordinary_scair_with_min() {
  local path="$1"
  local tile_size="$2"
  validate_runtime_product_with_min "$path" "$tile_size"
}

validate_value_dependent_no_min() {
  local path="$1"
  require_ir_pattern "$path" 'arith\.muli' \
    "value-dependent route must preserve shape-product provenance"
  require_ir_pattern "$path" 'cf\.assert' \
    "value-dependent route must carry the positive dynamic index assertion"
  require_ir_pattern "$path" 'd_affine\.for %[A-Za-z0-9_]+ = #[A-Za-z0-9_]+\(%[A-Za-z0-9_]+\) to #[A-Za-z0-9_]+\(%[A-Za-z0-9_]+\) step %[A-Za-z0-9_]+' \
    "value-dependent route must tile the product loop with a factor-derived dynamic step"
  reject_ir_pattern "$path" 'arith\.minsi| to min |affine\.min|d_affine\.min|remainder| mod|cleanup' \
    "value-dependent route must not use min/tail cleanup"
}

validate_value_dependent_static_affine_compatible() {
  local path="$1"
  local tile_size="$2"
  require_ir_pattern "$path" 'arith\.muli' \
    "static value-dependent route must preserve shape-product provenance"
  require_ir_pattern "$path" "affine_map<\\(d0\\)\\[\\] -> \\(d0 \\+ $tile_size\\)>|affine_map<\\(d0\\) -> \\(d0 \\+ $tile_size\\)>" \
    "static value-dependent route must encode the inner tile end as an affine constant offset"
  require_ir_pattern "$path" "affine\\.for %[A-Za-z0-9_]+ = #[A-Za-z0-9_]+\\(%[A-Za-z0-9_]+\\) to #[A-Za-z0-9_]+\\(%[A-Za-z0-9_]+\\) step $tile_size" \
    "static value-dependent route must use a stock affine outer loop with static tile step"
  require_ir_pattern "$path" "affine\\.for %[A-Za-z0-9_]+ = #[A-Za-z0-9_]+\\(%[A-Za-z0-9_]+\\) to #[A-Za-z0-9_]+\\(%[A-Za-z0-9_]+\\) step 1" \
    "static value-dependent route must keep a unit-step inner tile loop"
  reject_ir_pattern "$path" 'd_affine\.for|d_affine\.min' \
    "static value-dependent route must bridge eligible loops to stock affine"
  reject_ir_pattern "$path" 'arith\.addi|arith\.minsi| to min |affine\.min|remainder| mod|cleanup|step %' \
    "static value-dependent route must not need dynamic tile-end arithmetic or tail cleanup"
}

run_stock_mlir_checks() {
  local prefix="$1"
  local path="$2"
  local unroll_factor="$3"
  "$MLIR_OPT" --allow-unregistered-dialect "$path" > "$prefix.stock_parse.mlir"
  "$MLIR_OPT" --allow-unregistered-dialect --canonicalize "$path" > "$prefix.stock_canonicalize.mlir"
  "$MLIR_OPT" --allow-unregistered-dialect --affine-loop-normalize "$path" > "$prefix.stock_normalize.mlir"
  "$MLIR_OPT" --allow-unregistered-dialect --pass-pipeline="builtin.module(func.func(affine-loop-unroll{unroll-factor=$unroll_factor}))" "$path" > "$prefix.stock_unroll.mlir"
}

validate_stock_unroll_no_cleanup() {
  local path="$1"
  reject_ir_pattern "$path" 'affine\.min|arith\.minsi| to min |remainder| mod|cleanup' \
    "stock affine unroll output must not contain tail cleanup"
  reject_ir_pattern "$path" 'affine\.for %[A-Za-z0-9_]+ = .* step 1' \
    "stock affine unroll should fully unroll the exact unit-step tile loop"
  require_ir_pattern "$path" 'memref\.store' \
    "stock affine unroll output must retain the loop body"
}

build_mlir_route() {
  local variant="$1"
  local src="$2"
  local tile_size="$3"
  local prefix="$OUT_DIR/${variant}"
  cp "$src" "$prefix.input.mlir"
  "$MLIR_OPT" "$src" --affine-loop-tile=tile-size="$tile_size" > "$prefix.tiled.mlir"
}

build_scair_route() {
  local variant="$1"
  local src="$2"
  local passes="$3"
  local prefix="$OUT_DIR/${variant}"
  cp "$src" "$prefix.input.mlir"
  run_scair_opt -a -s "$src" --passes "$passes" > "$prefix.tiled.mlir"
}

append_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local variant="$3"
  local src="$4"
  local tiled="$5"
  local notes="$6"

  append_metrics_csv_row \
    "$metrics_csv" \
    "affine_tiling_benchmark" \
    "$BENCHMARK_NAME" \
    "$variant" \
    "$variant" \
    "ok" \
    "NA" \
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
    "NA" \
    "$(count_ops "$tiled")" \
    "$(count_ops_structural "$tiled")" \
    "$(file_metric lines "$tiled")" \
    "NA" \
    "NA" \
    "NA" \
    "NA" \
    "NA" \
    "NA" \
    "$notes"

  append_summary_row \
    "$summary_md" \
    "$BENCHMARK_NAME" \
    "$variant" \
    "$variant" \
    "ok" \
    "NA" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$tiled")" \
    "NA" \
    "NA" \
    "NA" \
    "NA"
}

METRICS_CSV="$OUT_DIR/metrics.csv"
METRICS_JSON="$OUT_DIR/metrics.json"
SUMMARY_MD="$OUT_DIR/summary.md"
write_route_manifest "$OUT_DIR/route_manifest.md"
write_metrics_csv_header "$METRICS_CSV"
write_summary_header "$SUMMARY_MD" "Affine Tiling Benchmark"

IFS=',' read -r -a SIZE_ENTRIES <<<"$AFFINE_TILING_SIZE_SET"
for size in "${SIZE_ENTRIES[@]}"; do
  if [[ ! "$size" =~ ^([1-9][0-9]*)x([1-9][0-9]*)$ ]]; then
    echo "error: invalid AFFINE_TILING_SIZE_SET entry '$size' (expected K0xK1, for example 16x3)" >&2
    exit 1
  fi
  k0="${BASH_REMATCH[1]}"
  k1="${BASH_REMATCH[2]}"
  if [[ "$k1" != "3" && "$AFFINE_TILING_ROUTES" == *"mlir_static_factor_reference"* ]]; then
    echo "error: mlir_static_factor_reference is intentionally fixed to static factor 3; use K1=3 or disable that route" >&2
    exit 1
  fi
  size_notes="size=k0=$k0;k1=$k1;k=$((k0 * k1))"

  if route_enabled "mlir_runtime_product"; then
    build_mlir_route "mlir_runtime_product" "$MLIR_RUNTIME_PRODUCT_SRC" "$k1"
    validate_runtime_product_with_min "$OUT_DIR/mlir_runtime_product.tiled.mlir" "$k1"
    append_row "$METRICS_CSV" "$SUMMARY_MD" "mlir_runtime_product" \
      "$OUT_DIR/mlir_runtime_product.input.mlir" \
      "$OUT_DIR/mlir_runtime_product.tiled.mlir" \
      "benchmark_role=minimal_claim;claim_role=ordinary_runtime_product;product_representation=arith.muli;tile_loop=product_loop;tile_step=static_$k1;tail_bound=$(tail_bound_kind "$OUT_DIR/mlir_runtime_product.tiled.mlir");exact_divisibility_proof=none;$size_notes"
  fi

  if route_enabled "mlir_static_factor_reference"; then
    build_mlir_route "mlir_static_factor_reference" "$MLIR_STATIC_FACTOR_SRC" "$k1"
    validate_static_factor_no_min "$OUT_DIR/mlir_static_factor_reference.tiled.mlir" "$k1"
    append_row "$METRICS_CSV" "$SUMMARY_MD" "mlir_static_factor_reference" \
      "$OUT_DIR/mlir_static_factor_reference.input.mlir" \
      "$OUT_DIR/mlir_static_factor_reference.tiled.mlir" \
      "benchmark_role=diagnostic;claim_role=static_affine_reference;product_representation=affine_static_factor;tile_loop=product_loop;tile_step=static_$k1;tail_bound=$(tail_bound_kind "$OUT_DIR/mlir_static_factor_reference.tiled.mlir");exact_divisibility_proof=static_affine_factor;$size_notes"
  fi

  if route_enabled "ordinary_scair_product_with_tail"; then
    build_scair_route "ordinary_scair_product_with_tail" "$ORDINARY_PRODUCT_SRC" "canonicalize,cse,dce,ordinary-affine-product-loop-tile-with-tail:$k1"
    validate_ordinary_scair_with_min "$OUT_DIR/ordinary_scair_product_with_tail.tiled.mlir" "$k1"
    append_row "$METRICS_CSV" "$SUMMARY_MD" "ordinary_scair_product_with_tail" \
      "$OUT_DIR/ordinary_scair_product_with_tail.input.mlir" \
      "$OUT_DIR/ordinary_scair_product_with_tail.tiled.mlir" \
      "benchmark_role=minimal_claim;claim_role=ordinary_runtime_product;product_representation=arith.muli;tile_loop=product_loop;tile_step=static_$k1;tail_bound=$(tail_bound_kind "$OUT_DIR/ordinary_scair_product_with_tail.tiled.mlir");exact_divisibility_proof=none;$size_notes"
  fi

  if route_enabled "value_dependent_exact_product"; then
    build_scair_route "value_dependent_exact_product" "$VALUE_DEP_PRODUCT_SRC" "canonicalize,cse,dce,canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps"
    validate_value_dependent_no_min "$OUT_DIR/value_dependent_exact_product.tiled.mlir"
    append_row "$METRICS_CSV" "$SUMMARY_MD" "value_dependent_exact_product" \
      "$OUT_DIR/value_dependent_exact_product.input.mlir" \
      "$OUT_DIR/value_dependent_exact_product.tiled.mlir" \
      "benchmark_role=minimal_claim;claim_role=value_dependent_exact_dynamic;product_representation=arith.muli;positivity_source=index_positive_assertion;tile_loop=product_loop;tile_step=dynamic_factor;tail_bound=$(tail_bound_kind "$OUT_DIR/value_dependent_exact_product.tiled.mlir");exact_divisibility_proof=arith.muli;$size_notes"
  fi

  if route_enabled "value_dependent_static_affine_compatible"; then
    if [[ "$k1" != "3" ]]; then
      echo "error: value_dependent_static_affine_compatible is intentionally fixed to index constant 3; use K1=3 or disable that route" >&2
      exit 1
    fi
    build_scair_route "value_dependent_static_affine_compatible" "$VALUE_DEP_STATIC_SRC" "canonicalize,cse,dce,canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,d-affine-to-affine-compatible,canonicalize,cse,dce"
    validate_value_dependent_static_affine_compatible "$OUT_DIR/value_dependent_static_affine_compatible.tiled.mlir" "$k1"
    run_stock_mlir_checks "$OUT_DIR/value_dependent_static_affine_compatible" "$OUT_DIR/value_dependent_static_affine_compatible.tiled.mlir" "$k1"
    validate_stock_unroll_no_cleanup "$OUT_DIR/value_dependent_static_affine_compatible.stock_unroll.mlir"
    append_row "$METRICS_CSV" "$SUMMARY_MD" "value_dependent_static_affine_compatible" \
      "$OUT_DIR/value_dependent_static_affine_compatible.input.mlir" \
      "$OUT_DIR/value_dependent_static_affine_compatible.tiled.mlir" \
      "benchmark_role=main_thesis_artifact;claim_role=value_dependent_static_affine_compatible;product_representation=arith.muli;tile_loop=product_loop;tile_step=static_$k1;tail_bound=$(tail_bound_kind "$OUT_DIR/value_dependent_static_affine_compatible.tiled.mlir");exact_divisibility_proof=arith.muli;stock_parse=ok;stock_verify=ok;stock_canonicalize=ok;stock_affine_loop_normalize=ok;stock_affine_loop_unroll_factor_$k1=ok;unroll_cleanup=none;$size_notes"
  fi
done

append_summary_metric_notes "$SUMMARY_MD"

python3 - "$METRICS_CSV" "$METRICS_JSON" <<'PY'
import csv
import json
import sys
from pathlib import Path

csv_path = Path(sys.argv[1])
json_path = Path(sys.argv[2])
payload = []
for row in csv.DictReader(csv_path.open(newline="", encoding="utf-8")):
    notes = {}
    for item in row["notes"].split(";"):
        if "=" in item:
            key, value = item.split("=", 1)
            notes[key] = value
    row["parsed_notes"] = notes
    payload.append(row)
json_path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
PY

echo
echo "Affine tiling benchmark complete."
echo "Produced:"
echo "  $OUT_DIR"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
