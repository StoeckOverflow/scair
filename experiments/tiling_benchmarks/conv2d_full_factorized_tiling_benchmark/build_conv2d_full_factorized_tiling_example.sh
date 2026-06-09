#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$ROOT/experiments/tiling_benchmarks/conv2d_full_factorized_tiling_benchmark"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
SCAIR_OPT="${SCAIR_OPT:-$ROOT/out/tools/opt/launcher.dest/run}"
LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
MLIR_OPT="${MLIR_OPT:-$LLVM_BUILD_DIR/bin/mlir-opt}"
MLIR_TRANSLATE="${MLIR_TRANSLATE:-$LLVM_BUILD_DIR/bin/mlir-translate}"
CONV2D_FULL_FACTORIZED_TILING_SIZE_SET="${CONV2D_FULL_FACTORIZED_TILING_SIZE_SET:-1x8x4x4x4x8x4x8x4x8x3x3}"
CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE="${CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE:-8}"
CONV2D_FULL_FACTORIZED_ROUTES="${CONV2D_FULL_FACTORIZED_ROUTES:-mlir_baseline_full_tile,ordinary_scair_full_tile_with_tail,dependent_full_guarded_tail_simplified,dependent_full_exact_tile}"

source "$ROOT/experiments/common_metrics.sh"
CONV2D_FULL_FACTORIZED_TILING_SIZE_SET="$(limit_csv_entries "$CONV2D_FULL_FACTORIZED_TILING_SIZE_SET")"
mkdir -p "$OUT_DIR"
require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"

MLIR_SRC="$EXAMPLE_DIR/conv2d_full_factorized_mlir_baseline.mlir"
ORDINARY_SRC="$EXAMPLE_DIR/conv2d_full_factorized_scair_ordinary.mlir"
DEPENDENT_SRC="$EXAMPLE_DIR/conv2d_full_factorized_scair_value_dependent.mlir"

route_enabled() {
  local route="$1" entry
  IFS=',' read -r -a routes <<<"$CONV2D_FULL_FACTORIZED_ROUTES"
  for entry in "${routes[@]}"; do
    [[ "$entry" == "$route" || "$entry" == "all" ]] && return 0
  done
  return 1
}
run_scair() { "$SCAIR_OPT" -s "$1" --passes "$2" > "$3"; }
tag_for() { echo "$1" | tr 'x' '_'; }
tail_kind() {
  if rg -q 'arith\.minsi' "$1"; then echo "arith_minsi"
  elif rg -q ' to min |affine\.min|d_affine\.min' "$1"; then echo "affine_min"
  else echo "none"; fi
}
count_pat() { (rg -o "$1" "$2" || true) | wc -l | tr -d ' '; }
require_pat() { if ! rg -q "$2" "$1"; then echo "error: $3: $1" >&2; exit 1; fi; }
reject_tail() { [[ "$(tail_kind "$1")" == "none" ]] || { echo "error: expected tail-free output: $1" >&2; exit 1; }; }

METRICS_CSV="$OUT_DIR/metrics.csv"
METRICS_JSON="$OUT_DIR/metrics.json"
SUMMARY_MD="$OUT_DIR/summary.md"

cat > "$METRICS_CSV" <<'CSV'
benchmark,variant,size,input,tiled,pipeline,tile_loops,output_tile_size,reduction_tile_size,product_representation,tail_bound,affine_min_count,arith_minsi_count,d_affine_for_count,affine_for_count,dynamic_step_count,mlir_loc,notes
CSV
cat > "$SUMMARY_MD" <<'MD'
# Conv2D Full-Factorized Tiling Benchmark Summary

| Variant | Size | Output tile | Reduction tile | Tail bound | affine.min | arith.minsi | d_affine.for | affine.for | Dynamic steps | Tiled IR |
| --- | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | --- |
MD

append_row() {
  local variant="$1" size="$2" red_tile="$3" input="$4" tiled="$5" pipeline="$6" product="$7" notes="$8"
  local tail affine_min arith_minsi d_affine_for affine_for dynamic_steps loc
  tail="$(tail_kind "$tiled")"
  affine_min="$(count_pat 'affine\.min| to min ' "$tiled")"
  arith_minsi="$(count_pat 'arith\.minsi' "$tiled")"
  d_affine_for="$(count_pat 'd_affine\.for' "$tiled")"
  affine_for="$(count_pat '(^|[^_])affine\.for' "$tiled")"
  dynamic_steps="$(count_pat 'step %[A-Za-z0-9_]+' "$tiled")"
  loc="$(file_metric lines "$tiled")"
  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "conv2d_full_factorized_tiling" "$variant" "$size" "$(basename "$input")" "$(basename "$tiled")" \
    "${pipeline//,/+}" "n+cout+oh+ow+p" "$CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE" "$red_tile" \
    "$product" "$tail" "$affine_min" "$arith_minsi" "$d_affine_for" "$affine_for" "$dynamic_steps" "$loc" "$notes" >> "$METRICS_CSV"
  printf '| `%s` | `%s` | %s | `%s` | `%s` | %s | %s | %s | %s | %s | `%s` |\n' \
    "$variant" "$size" "$CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE" "$red_tile" "$tail" "$affine_min" "$arith_minsi" "$d_affine_for" "$affine_for" "$dynamic_steps" "$(basename "$tiled")" >> "$SUMMARY_MD"
  if [[ "${CONV2D_EMIT_LLVM:-0}" == "1" ]]; then
    local prefix="${tiled%.tiled.mlir}"
    try_lower_d_memref_to_llvm_artifacts "$tiled" "$prefix.llvm.mlir" "$prefix.ll" "$prefix.llvm_status.txt"
  fi
}

IFS=',' read -r -a sizes <<<"$CONV2D_FULL_FACTORIZED_TILING_SIZE_SET"
for dims in "${sizes[@]}"; do
  IFS='x' read -r n0 n1 cin0 cin1 cout0 cout1 oh0 oh1 ow0 ow1 kh kw <<<"$dims"
  if [[ -z "${kw:-}" ]]; then
    echo "error: invalid CONV2D_FULL_FACTORIZED_TILING_SIZE_SET entry '$dims' (expected N0xN1xCin0xCin1xCout0xCout1xOH0xOH1xOW0xOW1xKhxKw)" >&2
    exit 1
  fi
  red_tile=$((cin1 * kh * kw))
  tag="$(tag_for "$dims")"
  if route_enabled "mlir_baseline_full_tile"; then
    out="$OUT_DIR/${tag}_mlir_baseline_full_tile.tiled.mlir"
    cp "$MLIR_SRC" "$OUT_DIR/${tag}_mlir_baseline_full_tile.input.mlir"
    "$MLIR_OPT" "$MLIR_SRC" --affine-loop-tile=tile-size="$CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE" > "$out"
    append_row "mlir_baseline_full_tile" "$dims" "$red_tile" "$MLIR_SRC" "$out" "mlir-opt --affine-loop-tile=tile-size=$CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE" "arith.muli_index" "tiling_decision=guarded;proof_source=none;upstream_reference_may_tile_only_legal_outer_bands"
  fi
  if route_enabled "ordinary_scair_full_tile_with_tail"; then
    out="$OUT_DIR/${tag}_ordinary_scair_full_tile_with_tail.tiled.mlir"
    cp "$ORDINARY_SRC" "$OUT_DIR/${tag}_ordinary_scair_full_tile_with_tail.input.mlir"
    pipeline="canonicalize,cse,dce,ordinary-affine-context-band-tile-with-tail:$CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE,ordinary-affine-product-loop-tile-with-tail:$red_tile,canonicalize,cse,dce"
    run_scair "$ORDINARY_SRC" "$pipeline" "$out"
    require_pat "$out" ' to min |affine\.min' "ordinary full route should keep tail/min"
    append_row "ordinary_scair_full_tile_with_tail" "$dims" "$red_tile" "$ORDINARY_SRC" "$out" "$pipeline" "arith.muli_index" "tiling_decision=guarded;proof_source=none;ordinary_output_and_reduction_tiling_keeps_guards"
  fi
  if route_enabled "dependent_full_guarded_tail_simplified"; then
    guarded="$OUT_DIR/${tag}_dependent_full_guarded_tail_simplified.guarded.mlir"
    out="$OUT_DIR/${tag}_dependent_full_guarded_tail_simplified.tiled.mlir"
    cp "$DEPENDENT_SRC" "$OUT_DIR/${tag}_dependent_full_guarded_tail_simplified.input.mlir"
    guarded_pipeline="canonicalize,cse,dce,canonicalize-d-tensor-size-products,dependent-context-band-factor-tile-with-tail,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce"
    pipeline="canonicalize,cse,dce,canonicalize-d-tensor-size-products,dependent-context-band-factor-tile-with-tail,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce"
    run_scair "$DEPENDENT_SRC" "$guarded_pipeline" "$guarded"
    require_pat "$guarded" 'arith\.minsi' "guarded dependent full route should emit min before simplification"
    run_scair "$DEPENDENT_SRC" "$pipeline" "$out"
    reject_tail "$out"
    append_row "dependent_full_guarded_tail_simplified" "$dims" "dynamic_Cin1KhKw" "$DEPENDENT_SRC" "$out" "$pipeline" "d_tensor.size.mul" "tiling_decision=guarded_then_exact_after_simplify;proof_source=size_product;guarded_artifact=$(basename "$guarded");proof_removes_output_and_reduction_tails"
  fi
  if route_enabled "dependent_full_exact_tile"; then
    out="$OUT_DIR/${tag}_dependent_full_exact_tile.tiled.mlir"
    cp "$DEPENDENT_SRC" "$OUT_DIR/${tag}_dependent_full_exact_tile.input.mlir"
    pipeline="canonicalize,cse,dce,canonicalize-d-tensor-size-products,dependent-context-band-exact-tile,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce"
    run_scair "$DEPENDENT_SRC" "$pipeline" "$out"
    reject_tail "$out"
    append_row "dependent_full_exact_tile" "$dims" "dynamic_Cin1KhKw" "$DEPENDENT_SRC" "$out" "$pipeline" "d_tensor.size.mul" "tiling_decision=exact;proof_source=size_product;diagnostic_direct_exact_full_factorized_tiling"
  fi
done

python3 - "$METRICS_CSV" "$METRICS_JSON" <<'PY'
import csv, json, sys
with open(sys.argv[1], newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f))
with open(sys.argv[2], 'w', encoding='utf-8') as f:
    json.dump(rows, f, indent=2)
PY

cat >> "$SUMMARY_MD" <<MD

## Interpretation

This benchmark composes output-space and reduction-domain factorization. Output
tiles are the parallel-scheduling substrate because they write disjoint \`Y\`
regions. Reduction tiles expose exact \`Cin1 * Kh * Kw\` chunks, but parallel
reduction would still need partial sums and accumulator privatization.
MD

echo "Conv2D full-factorized tiling benchmark complete."
echo "Produced: $METRICS_CSV $METRICS_JSON $SUMMARY_MD"
