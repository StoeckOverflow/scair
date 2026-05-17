#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-clean-build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
ITERATIONS="${ITERATIONS:-100}"
ATTENTION_MHA_ROUTES="${ATTENTION_MHA_ROUTES:-mlir_baseline,ordinary_scair_hidden_tile_with_tail,value_dependent_exact_tile}"
ATTENTION_MHA_TILE_SIZE="${ATTENTION_MHA_TILE_SIZE:-64}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_OPT="${MLIR_OPT:-$BIN_DIR/mlir-opt}"
MLIR_TRANSLATE="${MLIR_TRANSLATE:-$BIN_DIR/mlir-translate}"
MLIR_BASELINE_SRC="${MLIR_BASELINE_SRC:-$EXAMPLE_DIR/attention_kernel_mlir_baseline.mlir}"
MLIR_DRIVER_SRC="${MLIR_DRIVER_SRC:-$EXAMPLE_DIR/driver_mlir.c}"
SCAIR_BASELINE_SRC="${SCAIR_BASELINE_SRC:-$EXAMPLE_DIR/attention_kernel_scair_baseline.mlir}"
SCAIR_BASELINE_DRIVER_SRC="${SCAIR_BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline.c}"
ORDINARY_REFINED_SRC="${ORDINARY_REFINED_SRC:-$EXAMPLE_DIR/attention_kernel_scair_ordinary_index_refined.mlir}"
VALUE_DEP_SRC="${VALUE_DEP_SRC:-$EXAMPLE_DIR/attention_kernel_scair_value_dependent.mlir}"
VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver.c}"
HELPER_SRC="${HELPER_SRC:-$EXAMPLE_DIR/attention_helper.c}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
MLIR_AFFINE_TILE_ARGS="${MLIR_AFFINE_TILE_ARGS:---affine-loop-tile=tile-size=$ATTENTION_MHA_TILE_SIZE}"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"
SIZE_DESCRIPTOR="batch=1;seq=128;heads=12;head_dim=64"
COMPILER_FLAGS="${BENCH_OPT_FLAGS:--O2}"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$MLIR_BASELINE_SRC"
require_file "$MLIR_DRIVER_SRC"
require_file "$SCAIR_BASELINE_SRC"
require_file "$SCAIR_BASELINE_DRIVER_SRC"
require_file "$ORDINARY_REFINED_SRC"
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

tail_handling_present() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  if rg -q ' to min |affine\.min|d_affine\.min|arith\.minsi|remainder| mod|cleanup' "$path"; then
    echo "yes"
  else
    echo "no"
  fi
}

tail_bound_kind() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  if rg -q 'arith\.minsi' "$path"; then
    echo "arith.minsi"
  elif rg -q ' to min |affine\.min|d_affine\.min' "$path"; then
    echo "affine_min"
  else
    echo "none"
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

dynamic_step_present() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  if rg -q 'step %[A-Za-z0-9_]+ : index' "$path"; then
    echo "yes"
  else
    echo "no"
  fi
}

shared_tile_size() {
  if [[ "$ATTENTION_MHA_TILE_SIZE" == "64" ]]; then
    echo "yes"
  else
    echo "no"
  fi
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
  sed -nE 's/^[[:space:]]*(%[A-Za-z0-9_]+) = arith\.muli .* : index$/\1/p' "$path" | head -n 1
}

require_mlir_baseline_context_shape() {
  local path="$1"
  local product
  product="$(first_index_product_value "$path")"
  if [[ -z "$product" ]]; then
    echo "error: MLIR attention baseline must compute hidden with ordinary arith.muli in $path" >&2
    exit 1
  fi

  reject_ir_pattern \
    "$path" \
    'dtensor\.|d_memref\.|d_affine\.' \
    "MLIR attention baseline must stay stock ordinary MLIR/Affine"
  require_ir_pattern \
    "$path" \
    "affine\\.for %[A-Za-z0-9_]+ = 0 to $product iter_args" \
    "MLIR attention baseline must retain the original flattened hidden affine.for with iter_args"
  reject_ir_pattern \
    "$path" \
    "affine\\.for %[A-Za-z0-9_]+ = 0 to $product step $ATTENTION_MHA_TILE_SIZE" \
    "MLIR attention baseline context route must not be reported as the direct flattened-hidden tiled route"
}

require_ordinary_hidden_tile_with_tail_shape() {
  local path="$1"
  require_ir_pattern \
    "$path" \
    'arith\.muli' \
    "ordinary attention control must compute hidden with ordinary index multiplication"
  reject_ir_pattern \
    "$path" \
    'dtensor\.|d_memref\.|d_affine\.' \
    "ordinary attention control must not use value-dependent or dependent-affine IR"
  require_ir_pattern \
    "$path" \
    "affine\\.for %[A-Za-z0-9_]+ = #[A-Za-z0-9_]+\\(%[A-Za-z0-9_]+\\) to #[A-Za-z0-9_]+\\(\\)\\[%[A-Za-z0-9_]+\\] step $ATTENTION_MHA_TILE_SIZE iter_args" \
    "ordinary attention control must tile flattened hidden with the benchmark static tile step"
  require_ir_pattern \
    "$path" \
    ' to min ' \
    "ordinary attention control must keep an affine min tail bound"
}

require_value_dependent_exact_hidden_tile_shape() {
  local path="$1"
  require_ir_pattern \
    "$path" \
    'dtensor\.nat\.mul' \
    "value-dependent attention exact route must preserve natmul provenance"
  require_ir_pattern \
    "$path" \
    'dtensor\.shape\.to_index' \
    "value-dependent attention exact route must materialize factor-derived dynamic tile size"
  require_ir_pattern \
    "$path" \
    'd_affine\.for %[A-Za-z0-9_]+ = #[A-Za-z0-9_]+\(%[A-Za-z0-9_]+\) to #[A-Za-z0-9_]+\(%[A-Za-z0-9_]+\) step %[A-Za-z0-9_]+ : index iter_args' \
    "value-dependent attention exact route must tile flattened hidden with a dynamic step"
  reject_ir_pattern \
    "$path" \
    'arith\.minsi| to min |affine\.min|d_affine\.min|remainder| mod|cleanup' \
    "value-dependent attention exact route must not use min/tail cleanup"
}

run_scair_opt() {
  "$SCAIR_OPT" "$@" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)'
}

build_helper() {
  local obj_out="$1"
  local flags=()
  read -r -a flags <<<"$COMPILER_FLAGS"
  "$CC" "${flags[@]}" -c "$HELPER_SRC" -o "$obj_out"
}

optimize_lowered_mlir() {
  local raw_mlir="$1"
  local opt_mlir="$2"
  "$MLIR_OPT" "$raw_mlir" \
    --canonicalize \
    --cse \
    --symbol-dce \
    --reconcile-unrealized-casts \
    > "$opt_mlir"
}

compile_llvm_ir() {
  local llvm_ir="$1"
  local obj="$2"
  local flags=()
  read -r -a flags <<<"$COMPILER_FLAGS"
  "$CC" "${flags[@]}" -x ir "$llvm_ir" -c -o "$obj"
}

link_benchmark_exe() {
  local driver_src="$1"
  local obj="$2"
  local helper_obj="$3"
  local exe="$4"
  local variant="$5"
  local flags=()
  read -r -a flags <<<"$COMPILER_FLAGS"
  "$CC" "${flags[@]}" \
    -DBENCH_LABEL="\"$BENCHMARK_NAME\"" \
    -DVARIANT_LABEL="\"$variant\"" \
    "$driver_src" "$obj" "$helper_obj" -lm -o "$exe"
}

build_mlir_variant() {
  local variant="$1"
  local src="$2"
  local driver_src="$3"
  local prefix="$OUT_DIR/$variant"
  local input_ir="$prefix.input.mlir"
  local tiled_ir="$prefix.tiled.mlir"
  local lowered_mlir_raw="$prefix.llvm.raw.mlir"
  local lowered_mlir_converted="$prefix.llvm.converted.mlir"
  local lowered_mlir_opt="$prefix.llvm.opt.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
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
    > "$lowered_mlir_raw"
  optimize_lowered_mlir "$lowered_mlir_raw" "$lowered_mlir_opt"
  cp "$lowered_mlir_opt" "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_opt" > "$llvm_ir"
  compile_llvm_ir "$llvm_ir" "$obj"
  link_benchmark_exe "$driver_src" "$obj" "$helper_obj" "$exe" "$variant"

  end_ns=$(now_ns)
  {
    echo "build_status=ok"
    printf 'compile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")"
  } > "$build_metrics"

  run_benchmark_repeated "$output_txt" "$exe" "$ITERATIONS"
  cat "$build_metrics" >> "$output_txt"
}

build_scair_affine_then_mlir_variant() {
  local variant="$1"
  local src="$2"
  local pre_lower_pipeline="$3"
  local driver_src="$4"
  local prefix="$OUT_DIR/$variant"
  local input_ir="$prefix.input.mlir"
  local tiled_ir="$prefix.tiled.mlir"
  local lowered_mlir_raw="$prefix.llvm.raw.mlir"
  local lowered_mlir_converted="$prefix.llvm.converted.mlir"
  local lowered_mlir_opt="$prefix.llvm.opt.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
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

  run_scair_opt -s "$src" --passes "$pre_lower_pipeline" > "$tiled_ir"

  "$MLIR_OPT" "$tiled_ir" \
    --lower-affine \
    --convert-scf-to-cf \
    --expand-strided-metadata \
    --finalize-memref-to-llvm \
    --convert-arith-to-llvm \
    --convert-index-to-llvm \
    --convert-func-to-llvm \
    --convert-cf-to-llvm \
    --reconcile-unrealized-casts \
    > "$lowered_mlir_raw"
  optimize_lowered_mlir "$lowered_mlir_raw" "$lowered_mlir_opt"
  cp "$lowered_mlir_opt" "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_opt" > "$llvm_ir"
  compile_llvm_ir "$llvm_ir" "$obj"
  link_benchmark_exe "$driver_src" "$obj" "$helper_obj" "$exe" "$variant"

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
  local lowered_mlir_raw="$prefix.llvm.raw.mlir"
  local lowered_mlir_converted="$prefix.llvm.converted.mlir"
  local lowered_mlir_opt="$prefix.llvm.opt.mlir"
  local lowered_mlir="$prefix.llvm.mlir"
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
    run_scair_opt -s "$src" --passes "$pre_lower_pipeline" > "$tiled_ir"
  else
    cp "$input_ir" "$tiled_ir"
  fi

  run_scair_opt -s "$src" --passes "$pipeline,convert-func-to-llvm,convert-llvm-export-abi" > "$lowered_mlir_raw"
  "$MLIR_OPT" "$lowered_mlir_raw" \
    --convert-arith-to-llvm \
    --reconcile-unrealized-casts \
    > "$lowered_mlir_converted"
  optimize_lowered_mlir "$lowered_mlir_converted" "$lowered_mlir_opt"
  cp "$lowered_mlir_opt" "$lowered_mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$lowered_mlir_opt" > "$llvm_ir"
  compile_llvm_ir "$llvm_ir" "$obj"
  link_benchmark_exe "$driver_src" "$obj" "$helper_obj" "$exe" "$variant"

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

  require_mlir_baseline_context_shape \
    "$OUT_DIR/mlir_baseline.tiled.mlir"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "mlir_baseline" \
    "$OUT_DIR/mlir_baseline.input.mlir" \
    "$OUT_DIR/mlir_baseline.llvm.mlir" \
    "$OUT_DIR/mlir_baseline.ll" \
    "$OUT_DIR/mlir_baseline.output.txt" \
    "claim_role=context;claim_scope=stock_affine_context_flattened_hidden_iter_args_untiled;loop_transform=fixed_tile_with_cleanup;tile_loop=outer_affine_bands;context_tile_step=static_$ATTENTION_MHA_TILE_SIZE;claim_tile_step=NA;tile_step=static_$ATTENTION_MHA_TILE_SIZE;shared_tile_size=$(shared_tile_size);tile_size=$ATTENTION_MHA_TILE_SIZE;tile_size_source=mlir_arg;product_representation=arith.muli_index;context_tail_bound=affine_min;claim_tail_bound=NA;tail_bound=affine_min_outer_bands_only;exact_divisibility_proof=none;dynamic_step_present=$(dynamic_step_present "$OUT_DIR/mlir_baseline.tiled.mlir");tail_cleanup_present=$(tail_handling_present "$OUT_DIR/mlir_baseline.tiled.mlir");tail_handling_present=$(tail_handling_present "$OUT_DIR/mlir_baseline.tiled.mlir");affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/mlir_baseline.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/mlir_baseline.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/mlir_baseline.tiled.mlir")"
fi

if route_enabled "ordinary_scair_hidden_tile_with_tail"; then
  echo "==> Building ordinary ScaIR flattened-hidden tile tail-control MHA kernel"
  build_scair_affine_then_mlir_variant \
    "ordinary_scair_hidden_tile_with_tail" \
    "$ORDINARY_REFINED_SRC" \
    "canonicalize,cse,dce,ordinary-affine-context-band-tile-with-tail:$ATTENTION_MHA_TILE_SIZE,ordinary-affine-product-tile-with-tail:$ATTENTION_MHA_TILE_SIZE" \
    "$MLIR_DRIVER_SRC"

  require_ordinary_hidden_tile_with_tail_shape \
    "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "ordinary_scair_hidden_tile_with_tail" \
    "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.input.mlir" \
    "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.llvm.mlir" \
    "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.ll" \
    "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.output.txt" \
    "claim_role=direct_control;claim_scope=ordinary_affine_product_tiles_flattened_hidden_with_static_benchmark_head_dim_and_keeps_min_tail;loop_transform=context_band_tile_with_tail+ordinary_affine_product_tile_with_tail;tile_loop=flattened_hidden;context_tile_step=static_$ATTENTION_MHA_TILE_SIZE;claim_tile_step=static_$ATTENTION_MHA_TILE_SIZE;tile_step=static_$ATTENTION_MHA_TILE_SIZE;shared_tile_size=$(shared_tile_size);tile_size=$ATTENTION_MHA_TILE_SIZE;tile_size_source=benchmark_static_head_dim;product_representation=arith.muli_index;context_tail_bound=affine_min;claim_tail_bound=affine_min;tail_bound=$(tail_bound_kind "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir");exact_divisibility_proof=none;dynamic_step_present=$(dynamic_step_present "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir");tail_cleanup_present=$(tail_handling_present "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir");tail_handling_present=$(tail_handling_present "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir");affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/ordinary_scair_hidden_tile_with_tail.tiled.mlir");tail_free_factorized=no"
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
    "claim_role=diagnostic;claim_scope=scair_dynamic_memref_baseline;loop_transform=none;tile_loop=none;tile_step=NA;shared_tile_size=no;tile_size=untiled;tile_size_source=NA;product_representation=arith.muli_index;tail_bound=NA;exact_divisibility_proof=none;dynamic_step_present=$(dynamic_step_present "$OUT_DIR/scair_baseline.tiled.mlir");tail_cleanup_present=$(tail_handling_present "$OUT_DIR/scair_baseline.tiled.mlir");tail_handling_present=$(tail_handling_present "$OUT_DIR/scair_baseline.tiled.mlir");affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/scair_baseline.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/scair_baseline.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/scair_baseline.tiled.mlir")"
fi

if route_enabled "value_dependent" || route_enabled "value_dependent_exact_tile"; then
  value_dep_variant="value_dependent_exact_tile"
  if route_enabled "value_dependent" && ! route_enabled "value_dependent_exact_tile"; then
    value_dep_variant="value_dependent"
  fi
  echo "==> Building ScaIR standard MHA value-dependent exact-tile kernel"
  build_scair_variant \
    "$value_dep_variant" \
    "$VALUE_DEP_SRC" \
    "canonicalize,cse,dce,canonicalize-dtensor-nat-products,dependent-context-band-exact-tile,dependent-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce,lower-dmemref-to-llvm" \
    "canonicalize,cse,dce,canonicalize-dtensor-nat-products,dependent-context-band-exact-tile,dependent-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce" \
    "$VALUE_DEP_DRIVER_SRC"

  require_value_dependent_exact_hidden_tile_shape \
    "$OUT_DIR/${value_dep_variant}.tiled.mlir"

  append_row \
    "$METRICS_CSV" \
    "$SUMMARY_MD" \
    "$value_dep_variant" \
    "$OUT_DIR/${value_dep_variant}.input.mlir" \
    "$OUT_DIR/${value_dep_variant}.llvm.mlir" \
    "$OUT_DIR/${value_dep_variant}.ll" \
    "$OUT_DIR/${value_dep_variant}.output.txt" \
    "claim_role=direct_value_dependent;claim_scope=dependent_natmul_guides_tail_free_exact_tiling_for_flattened_hidden_dynamic_head_dim_case;loop_transform=dependent_context_band_exact_tile+dependent_exact_tile;tile_loop=flattened_hidden;context_tile_step=NA;claim_tile_step=dynamic_head_dim;tile_step=dynamic_head_dim;shared_tile_size=$(shared_tile_size);tile_size=64;tile_size_source=posnat_head_dim;positivity_source=posnat_type;product_representation=dtensor.nat.mul;context_tail_bound=NA;claim_tail_bound=none;tail_bound=none;exact_divisibility_proof=dtensor.nat.mul;dynamic_step_present=$(dynamic_step_present "$OUT_DIR/${value_dep_variant}.tiled.mlir");tail_cleanup_present=$(tail_handling_present "$OUT_DIR/${value_dep_variant}.tiled.mlir");tail_handling_present=$(tail_handling_present "$OUT_DIR/${value_dep_variant}.tiled.mlir");affine_cleanup_present=$(affine_cleanup_present "$OUT_DIR/${value_dep_variant}.tiled.mlir");factorized_tile_count=$(factorized_tile_count "$OUT_DIR/${value_dep_variant}.tiled.mlir");tail_free_factorized=$(tail_free_factorized "$OUT_DIR/${value_dep_variant}.tiled.mlir")"
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
            "claim_role": notes.get("claim_role", "NA"),
            "claim_scope": notes.get("claim_scope", "NA"),
            "loop_transform": notes.get("loop_transform", "NA"),
            "tile_loop": notes.get("tile_loop", "NA"),
            "context_tile_step": notes.get("context_tile_step", "NA"),
            "claim_tile_step": notes.get("claim_tile_step", "NA"),
            "tile_step": notes.get("tile_step", "NA"),
            "shared_tile_size": notes.get("shared_tile_size", "NA"),
            "tile_size": notes.get("tile_size", "NA"),
            "tile_size_source": notes.get("tile_size_source", "NA"),
            "product_representation": notes.get("product_representation", "NA"),
            "context_tail_bound": notes.get("context_tail_bound", "NA"),
            "claim_tail_bound": notes.get("claim_tail_bound", "NA"),
            "tail_bound": notes.get("tail_bound", "NA"),
            "dynamic_step_present": notes.get("dynamic_step_present", "NA"),
            "tail_cleanup_present": notes.get("tail_cleanup_present", "NA"),
            "tail_handling_present": notes.get("tail_handling_present", "NA"),
            "affine_cleanup_present": notes.get("affine_cleanup_present", "NA"),
            "factorized_tile_count": notes.get("factorized_tile_count", "NA"),
            "tail_free_factorized": notes.get("tail_free_factorized", "NA"),
            "exact_divisibility_proof": notes.get("exact_divisibility_proof", "NA"),
            "positivity_source": notes.get("positivity_source", "NA"),
        }
    )

json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
PY

echo
echo "Attention MHA benchmark build complete."
echo "Produced:"
echo "  enabled route artifacts, including *.llvm.raw.mlir and *.llvm.opt.mlir, under $OUT_DIR"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
echo "  $OUT_DIR/summary.md"
