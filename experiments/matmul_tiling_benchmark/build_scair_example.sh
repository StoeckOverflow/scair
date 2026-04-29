#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${MATMUL_TILING_ITERATIONS:-${ITERATIONS:-100}}"
MATMUL_TILING_SIZE_SET="${MATMUL_TILING_SIZE_SET:-128x128x12x64,128x128x16x32,256x128x12x64}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="$BIN_DIR/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"
MLIR_AFFINE_TILE_ARGS="${MLIR_AFFINE_TILE_ARGS:---affine-loop-tile=tile-size=32}"
MLIR_BASELINE_SRC="${MLIR_BASELINE_SRC:-$EXAMPLE_DIR/matmul_kernel_mlir_baseline.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver_mlir.c}"
VALUE_DEP_SRC="${VALUE_DEP_SRC:-$EXAMPLE_DIR/matmul_kernel_scair_value_dependent.mlir}"
VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
COMPILER_FLAGS="-O2"
BENCHMARK_NAME="matmul_tiling"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$MLIR_BASELINE_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$VALUE_DEP_SRC"
require_file "$VALUE_DEP_DRIVER_SRC"

size_tag() {
  echo "$1" | tr 'x' '_'
}

size_descriptor() {
  local m="$1"
  local n="$2"
  local k0="$3"
  local k1="$4"
  local k=$((k0 * k1))
  echo "m=$m;n=$n;k0=$k0;k1=$k1;k=$k"
}

tail_handling_present() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  if rg -q ' to min |affine\.min|d_affine\.min|remainder| mod|cleanup' "$path"; then
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
  if rg -q 'step 1 : i32' "$path" && ! rg -q ' to min |affine\.min|d_affine\.min|remainder| mod|cleanup' "$path"; then
    echo "yes"
  else
    echo "no"
  fi
}

run_scair_opt() {
  "$SCAIR_OPT" "$@" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)'
}

build_mlir_variant() {
  local variant="$1"
  local src="$2"
  local driver_src="$3"
  local artifact_tag="$4"
  local m="$5"
  local n="$6"
  local k0="$7"
  local k1="$8"
  local prefix="$OUT_DIR/${artifact_tag}_${variant}"
  local input_ir="$prefix.input.mlir"
  local tiled_ir="$prefix.tiled.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
  local llvm_ir="$prefix.ll"
  local obj="$prefix.o"
  local exe="$prefix.exec"
  local output_txt="$prefix.output.txt"
  local build_metrics="$prefix.build_metrics.txt"
  local start_ns
  local end_ns

  cp "$src" "$input_ir"
  start_ns=$(now_ns)

  "$MLIR_OPT" "$src" \
    "$MLIR_AFFINE_TILE_ARGS" \
    > "$tiled_ir"

  "$MLIR_OPT" "$src" \
    --affine-loop-tile=tile-size=32 \
    --lower-affine \
    --convert-scf-to-cf \
    --expand-strided-metadata \
    --finalize-memref-to-llvm \
    --convert-arith-to-llvm \
    --convert-index-to-llvm \
    --convert-cf-to-llvm \
    --convert-func-to-llvm \
    --reconcile-unrealized-casts \
    > "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir" > "$llvm_ir"
  "$CC" -O2 -x ir "$llvm_ir" -c -o "$obj"
  "$CC" -O2 \
    -DBENCH_LABEL="\"$BENCHMARK_NAME\"" \
    -DVARIANT_LABEL="\"$variant\"" \
    -DMATMUL_TILING_M="$m" \
    -DMATMUL_TILING_N="$n" \
    -DMATMUL_TILING_K0="$k0" \
    -DMATMUL_TILING_K1="$k1" \
    "$driver_src" "$obj" -o "$exe"

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
  local artifact_tag="$6"
  local m="$7"
  local n="$8"
  local k0="$9"
  local k1="${10}"
  local prefix="$OUT_DIR/${artifact_tag}_${variant}"
  local input_ir="$prefix.input.mlir"
  local tiled_ir="$prefix.tiled.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
  local llvm_ir="$prefix.ll"
  local obj="$prefix.o"
  local exe="$prefix.exec"
  local output_txt="$prefix.output.txt"
  local build_metrics="$prefix.build_metrics.txt"
  local start_ns
  local end_ns

  cp "$src" "$input_ir"
  start_ns=$(now_ns)

  if [[ -n "$pre_lower_pipeline" ]]; then
    run_scair_opt -s "$src" --passes "$pre_lower_pipeline" > "$tiled_ir"
  else
    cp "$input_ir" "$tiled_ir"
  fi

  run_scair_opt -s "$src" --passes "$pipeline,convert-func-to-llvm,convert-llvm-export-abi" > "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir" > "$llvm_ir"
  "$CC" -O2 -x ir "$llvm_ir" -c -o "$obj"
  "$CC" -O2 \
    -DBENCH_LABEL="\"$BENCHMARK_NAME\"" \
    -DVARIANT_LABEL="\"$variant\"" \
    -DMATMUL_TILING_M="$m" \
    -DMATMUL_TILING_N="$n" \
    -DMATMUL_TILING_K0="$k0" \
    -DMATMUL_TILING_K1="$k1" \
    "$driver_src" "$obj" -o "$exe"

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
  local row_size_descriptor="$9"

  append_metrics_csv_row \
    "$metrics_csv" \
    "matmul_tiling_benchmark" \
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
    "matmul_tiling" \
    "$row_size_descriptor" \
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
    "$row_size_descriptor" \
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

write_summary_header "$SUMMARY_MD" "Matmul Factorization-Aware Tiling Benchmark Summary"
printf '%s\n' "$COMMON_METRICS_HEADER" > "$METRICS_CSV"

IFS=',' read -r -a MATMUL_TILING_SIZES <<<"$MATMUL_TILING_SIZE_SET"
for dims in "${MATMUL_TILING_SIZES[@]}"; do
  IFS='x' read -r m n k0 k1 <<<"$dims"
  if [[ -z "${m:-}" || -z "${n:-}" || -z "${k0:-}" || -z "${k1:-}" ]]; then
    echo "error: invalid MATMUL_TILING_SIZE_SET entry '$dims' (expected MxNxK0xK1)" >&2
    exit 1
  fi

  artifact_tag="$(size_tag "$dims")"
  row_size_descriptor="$(size_descriptor "$m" "$n" "$k0" "$k1")"

  echo "==> Building upstream MLIR matmul tiling baseline for $row_size_descriptor"
  build_mlir_variant \
    "mlir_baseline" \
    "$MLIR_BASELINE_SRC" \
    "$MLIR_DRIVER_SRC" \
    "$artifact_tag" \
    "$m" \
    "$n" \
    "$k0" \
    "$k1"

  echo "==> Building value-dependent matmul tiling kernel for $row_size_descriptor"
  build_scair_variant \
    "value_dependent" \
    "$VALUE_DEP_SRC" \
    "dtensor-to-dmemref-shape-preserving,canonicalize,cse,dce,attention-factorization-aware-dependent-tiling,lower-dmemref-to-llvm" \
    "dtensor-to-dmemref-shape-preserving,canonicalize,cse,dce,attention-factorization-aware-dependent-tiling" \
    "$VALUE_DEP_DRIVER_SRC" \
    "$artifact_tag" \
    "$m" \
    "$n" \
    "$k0" \
    "$k1"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "mlir_baseline" \
    "$OUT_DIR/${artifact_tag}_mlir_baseline.input.mlir" \
    "$OUT_DIR/${artifact_tag}_mlir_baseline.llvm.mlir" \
    "$OUT_DIR/${artifact_tag}_mlir_baseline.ll" \
    "$OUT_DIR/${artifact_tag}_mlir_baseline.output.txt" \
    "benchmark_class=structural_codegen_supporting_runtime;factorization=K0*K1;claim_scope=baseline_has_index_product_bound_but_no_dependent_natmul_provenance;timed_region=output_reset+kernel;tail_handling_present=$(tail_handling_present "$OUT_DIR/${artifact_tag}_mlir_baseline.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/${artifact_tag}_mlir_baseline.tiled.mlir");tail_free_factorized=no" \
    "$row_size_descriptor"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "value_dependent" \
    "$OUT_DIR/${artifact_tag}_value_dependent.input.mlir" \
    "$OUT_DIR/${artifact_tag}_value_dependent.llvm.mlir" \
    "$OUT_DIR/${artifact_tag}_value_dependent.ll" \
    "$OUT_DIR/${artifact_tag}_value_dependent.output.txt" \
    "benchmark_class=structural_codegen_supporting_runtime;factorization=K0*K1;claim_scope=dependent_natmul_guides_tail_free_reduction_tiling_in_tested_case;timed_region=output_reset+kernel;tail_handling_present=$(tail_handling_present "$OUT_DIR/${artifact_tag}_value_dependent.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/${artifact_tag}_value_dependent.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/${artifact_tag}_value_dependent.tiled.mlir")" \
    "$row_size_descriptor"
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
    payload.append(
        {
            "size": row["size"],
            "variant": row["variant"],
            "run_status": row["run_status"],
            "runtime_median_ns_per_iter": row["runtime_median_ns_per_iter"],
            "runtime_iqr_ns_per_iter": row["runtime_iqr_ns_per_iter"],
            "benchmark_repetitions": row["benchmark_repetitions"],
            "tail_handling_present": notes.get("tail_handling_present", "NA"),
            "factorized_tile_count": notes.get("factorized_tile_count", "NA"),
            "tail_free_factorized": notes.get("tail_free_factorized", "NA"),
        }
    )

json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
PY

echo
echo "Matmul tiling benchmark build complete."
echo "Produced:"
echo "  per-size *.input.mlir, *.tiled.mlir, *.llvm.mlir, *.ll, and executables under $OUT_DIR"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
echo "  $OUT_DIR/summary.md"
