#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-100}"
ATTENTION_MHA_ROUTES="${ATTENTION_MHA_ROUTES:-mlir_baseline,scair_baseline,value_dependent}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="$BIN_DIR/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
MLIR_BASELINE_SRC="${MLIR_BASELINE_SRC:-$EXAMPLE_DIR/attention_kernel_mlir_baseline.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver_mlir.c}"
SCAIR_BASELINE_SRC="${SCAIR_BASELINE_SRC:-$EXAMPLE_DIR/attention_kernel_scair_baseline.mlir}"
SCAIR_BASELINE_DRIVER_SRC="${SCAIR_BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline.c}"
VALUE_DEP_SRC="${VALUE_DEP_SRC:-$EXAMPLE_DIR/attention_kernel_scair_value_dependent.mlir}"
VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"
HELPER_SRC="${HELPER_SRC:-$EXAMPLE_DIR/attention_helper.c}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
MLIR_AFFINE_TILE_ARGS="${MLIR_AFFINE_TILE_ARGS:---affine-loop-tile=tile-size=32}"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
SIZE_DESCRIPTOR="batch=1;seq=128;heads=12;head_dim=64"
COMPILER_FLAGS="-O2"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$MLIR_BASELINE_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$SCAIR_BASELINE_SRC"
require_file "$SCAIR_BASELINE_DRIVER_SRC"
require_file "$VALUE_DEP_SRC"
require_file "$VALUE_DEP_DRIVER_SRC"
require_file "$HELPER_SRC"

BENCHMARK_NAME="attention_mha"

route_enabled() {
  local route="$1"
  local entry
  IFS=',' read -r -a ATTENTION_MHA_ROUTE_LIST <<<"$ATTENTION_MHA_ROUTES"
  for entry in "${ATTENTION_MHA_ROUTE_LIST[@]}"; do
    if [[ "$entry" == "$route" || "$entry" == "all" ]]; then
      return 0
    fi
  done
  return 1
}

affine_cleanup_present() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  if rg -q ' to min ' "$path"; then
    echo "yes"
  else
    echo "no"
  fi
}

factorized_tile_count() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count="$(rg -o 'step 1 : i32' "$path" | wc -l | tr -d ' ')"
  echo "${count:-0}"
}

tail_free_factorized() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  if rg -q 'step 1 : i32' "$path"; then
    echo "yes"
  else
    echo "no"
  fi
}

build_helper() {
  local obj_out="$1"
  "$CC" -O2 -c "$HELPER_SRC" -o "$obj_out"
}

build_mlir_variant() {
  local variant="$1"
  local src="$2"
  local driver_src="$3"
  local prefix="$OUT_DIR/$variant"
  local input_ir="$prefix.input.mlir"
  local tiled_ir="$prefix.tiled.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
  local lowered_mlir_raw="$prefix.llvm.raw.mlir"
  local llvm_ir="$prefix.ll"
  local obj="$prefix.o"
  local exe="$prefix.exec"
  local output_txt="$prefix.output.txt"
  local build_metrics="$prefix.build_metrics.txt"
  local helper_obj="$OUT_DIR/attention_helper.o"
  local start_ns
  local end_ns

  cp "$src" "$input_ir"
  build_helper "$helper_obj"
  start_ns=$(now_ns)

  "$MLIR_OPT" "$src" \
    "$MLIR_AFFINE_TILE_ARGS" \
    > "$tiled_ir"

  "$MLIR_OPT" "$src" \
    "$MLIR_AFFINE_TILE_ARGS" \
    --lower-affine \
    --convert-scf-to-cf \
    --expand-strided-metadata \
    --finalize-memref-to-llvm \
    --convert-arith-to-llvm \
    --convert-index-to-llvm \
    --convert-func-to-llvm \
    --convert-cf-to-llvm \
    --reconcile-unrealized-casts \
    > "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir" > "$llvm_ir"
  "$CC" -O2 -x ir "$llvm_ir" -c -o "$obj"
  "$CC" -O2 \
    -DBENCH_LABEL="\"$BENCHMARK_NAME\"" \
    -DVARIANT_LABEL="\"$variant\"" \
    "$driver_src" "$obj" "$helper_obj" -lm -o "$exe"

  end_ns=$(now_ns)
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } > "$build_metrics"

  run_benchmark_repeated "$output_txt" "$exe" "$ITERATIONS"
  cat "$build_metrics" >> "$output_txt"
}

build_scair_variant() {
  local variant="$1"
  local src="$2"
  local pipeline="$3"
  local pre_lower_pipeline="$4"
  local driver_src="$5"
  local prefix="$OUT_DIR/$variant"
  local input_ir="$prefix.input.mlir"
  local tiled_ir="$prefix.tiled.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
  local lowered_mlir_raw="$prefix.llvm.raw.mlir"
  local llvm_ir="$prefix.ll"
  local obj="$prefix.o"
  local exe="$prefix.exec"
  local output_txt="$prefix.output.txt"
  local build_metrics="$prefix.build_metrics.txt"
  local helper_obj="$OUT_DIR/attention_helper.o"
  local start_ns
  local end_ns

  cp "$src" "$input_ir"
  build_helper "$helper_obj"
  start_ns=$(now_ns)

  if [[ -n "$pre_lower_pipeline" ]]; then
    "$SCAIR_OPT" -s "$src" --passes "$pre_lower_pipeline" > "$tiled_ir"
  else
    cp "$input_ir" "$tiled_ir"
  fi

  "$SCAIR_OPT" -s "$src" --passes "$pipeline,convert-func-to-llvm,convert-llvm-export-abi" > "$lowered_mlir_raw"
  "$MLIR_OPT" "$lowered_mlir_raw" \
    --convert-arith-to-llvm \
    --reconcile-unrealized-casts \
    > "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir" > "$llvm_ir"
  "$CC" -O2 -x ir "$llvm_ir" -c -o "$obj"
  "$CC" -O2 \
    -DBENCH_LABEL="\"$BENCHMARK_NAME\"" \
    -DVARIANT_LABEL="\"$variant\"" \
    "$driver_src" "$obj" "$helper_obj" -lm -o "$exe"

  end_ns=$(now_ns)
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } > "$build_metrics"

  run_benchmark_repeated "$output_txt" "$exe" "$ITERATIONS"
  cat "$build_metrics" >> "$output_txt"
}

append_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local variant="$3"
  local src="$4"
  local lowered_mlir="$5"
  local llvm_ir="$6"
  local output_txt="$7"
  local notes="$8"

  append_metrics_csv_row \
    "$metrics_csv" \
    "attention_mha_benchmark" \
    "$BENCHMARK_NAME" \
    "$variant" \
    "$variant" \
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
    "$notes" \
    "$(count_source_helpers "$src")" \
    "0" \
    "0" \
    "NA" \
    "NA" \
    "attention_mha" \
    "$SIZE_DESCRIPTOR" \
    "$variant" \
    "NA" \
    "NA" \
    "NA" \
    "$(metric_field compile_ms "$output_txt")" \
    "$(metric_field ns_per_iter "$output_txt")" \
    "$(metric_field runtime_iqr_ns_per_iter "$output_txt")" \
    "$(metric_field benchmark_repetitions "$output_txt")" \
    "$(metric_field checksum "$output_txt")" \
    "$(metric_field checksum_status "$output_txt")" \
    "$COMPILER_FLAGS" \
    "$GIT_COMMIT" \
    "$RUN_DATE" \
    "$MACHINE_ID" \
    "$ENV_PATH" \
    "$(metric_field raw_timings_path "$output_txt")"

  append_summary_row \
    "$summary_md" \
    "$BENCHMARK_NAME" \
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

SUMMARY_MD="$OUT_DIR/summary.md"
METRICS_CSV="$OUT_DIR/metrics.csv"
METRICS_JSON="$OUT_DIR/metrics.json"

write_summary_header "$SUMMARY_MD" "Attention MHA Benchmark Summary"
printf '%s\n' "$COMMON_METRICS_HEADER" > "$METRICS_CSV"

if route_enabled "mlir_baseline"; then
  echo "==> Building upstream MLIR standard MHA baseline"
  build_mlir_variant \
    "mlir_baseline" \
    "$MLIR_BASELINE_SRC" \
    "$MLIR_DRIVER_SRC"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "mlir_baseline" \
    "$OUT_DIR/mlir_baseline.input.mlir" \
    "$OUT_DIR/mlir_baseline.llvm.mlir" \
    "$OUT_DIR/mlir_baseline.ll" \
    "$OUT_DIR/mlir_baseline.output.txt" \
    "claim_scope=upstream_mlir_baseline;affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/mlir_baseline.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/mlir_baseline.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/mlir_baseline.tiled.mlir")"
fi

if route_enabled "scair_baseline"; then
  echo "==> Building ScaIR standard MHA dynamic baseline"
  build_scair_variant \
    "scair_baseline" \
    "$SCAIR_BASELINE_SRC" \
    "lower-dynamic-memref-to-llvm-baseline" \
    "canonicalize,cse,dce" \
    "$SCAIR_BASELINE_DRIVER_SRC"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "scair_baseline" \
    "$OUT_DIR/scair_baseline.input.mlir" \
    "$OUT_DIR/scair_baseline.llvm.mlir" \
    "$OUT_DIR/scair_baseline.ll" \
    "$OUT_DIR/scair_baseline.output.txt" \
    "claim_scope=scair_dynamic_memref_baseline;affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/scair_baseline.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/scair_baseline.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/scair_baseline.tiled.mlir")"
fi

if route_enabled "value_dependent"; then
  echo "==> Building ScaIR standard MHA value-dependent kernel"
  build_scair_variant \
    "value_dependent" \
    "$VALUE_DEP_SRC" \
    "attention-factorization-aware-dependent-tiling,canonicalize,cse,lower-dmemref-to-llvm" \
    "attention-factorization-aware-dependent-tiling,canonicalize,cse" \
    "$VALUE_DEP_DRIVER_SRC"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "value_dependent" \
    "$OUT_DIR/value_dependent.input.mlir" \
    "$OUT_DIR/value_dependent.llvm.mlir" \
    "$OUT_DIR/value_dependent.ll" \
    "$OUT_DIR/value_dependent.output.txt" \
    "claim_scope=dependent_natmul_provenance;affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/value_dependent.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/value_dependent.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/value_dependent.tiled.mlir")"
fi

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
    payload.append(
        {
            "variant": row["variant"],
            "run_status": row["run_status"],
            "runtime_median_ns_per_iter": row["runtime_median_ns_per_iter"],
            "runtime_iqr_ns_per_iter": row["runtime_iqr_ns_per_iter"],
            "benchmark_repetitions": row["benchmark_repetitions"],
            "affine_cleanup_present": notes.get("affine_cleanup_present", "NA"),
            "factorized_tile_count": notes.get("factorized_tile_count", "NA"),
            "tail_free_factorized": notes.get("tail_free_factorized", "NA"),
        }
    )

json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
PY

echo
echo "Attention MHA benchmark build complete."
echo "Produced:"
echo "  enabled route artifacts under $OUT_DIR"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
echo "  $OUT_DIR/summary.md"
