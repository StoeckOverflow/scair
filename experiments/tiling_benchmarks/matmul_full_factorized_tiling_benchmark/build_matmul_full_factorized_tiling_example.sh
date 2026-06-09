#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"
MLIR_OPT="${MLIR_OPT:-$BIN_DIR/mlir-opt}"
MLIR_TRANSLATE="${MLIR_TRANSLATE:-$BIN_DIR/mlir-translate}"
ITERATIONS="${MATMUL_FULL_FACTORIZED_TILING_ITERATIONS:-${ITERATIONS:-30}}"
MATMUL_FULL_FACTORIZED_TILING_SIZE_SET="${MATMUL_FULL_FACTORIZED_TILING_SIZE_SET:-2x64x2x64x12x64,3x64x2x64x16x32}"
MATMUL_FULL_FACTORIZED_TILE_SIZE="${MATMUL_FULL_FACTORIZED_TILE_SIZE:-64}"
MATMUL_FULL_FACTORIZED_ROUTES="${MATMUL_FULL_FACTORIZED_ROUTES:-mlir_baseline_full_tile,ordinary_scair_full_tile_with_tail,dependent_full_guarded_tail_simplified,dependent_full_exact_tile}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"
MATMUL_FULL_FACTORIZED_TILING_SIZE_SET="$(limit_csv_entries "$MATMUL_FULL_FACTORIZED_TILING_SIZE_SET")"

SCAIR_OPT="${SCAIR_OPT:-$SCAIR_ROOT/out/tools/opt/launcher.dest/run}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"
BENCHMARK_NAME="matmul_full_factorized_tiling"
COMPILER_FLAGS="-O2"

MLIR_SRC="$EXAMPLE_DIR/matmul_full_factorized_mlir_baseline.mlir"
ORDINARY_SRC="$EXAMPLE_DIR/matmul_full_factorized_scair_ordinary.mlir"
DEPENDENT_SRC="$EXAMPLE_DIR/matmul_full_factorized_scair_value_dependent.mlir"
MLIR_DRIVER="$EXAMPLE_DIR/driver_mlir.c"
ORDINARY_DRIVER="$EXAMPLE_DIR/driver_ordinary.c"
DEPENDENT_DRIVER="$EXAMPLE_DIR/driver_dependent.c"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"

require_bin "$SCAIR_OPT"
require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"
require_file "$MLIR_SRC"
require_file "$ORDINARY_SRC"
require_file "$DEPENDENT_SRC"
require_file "$MLIR_DRIVER"
require_file "$ORDINARY_DRIVER"
require_file "$DEPENDENT_DRIVER"

run_scair_opt() {
  "$SCAIR_OPT" "$@" \
    | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)'
}

route_enabled() {
  local route="$1"
  local entry
  IFS=',' read -r -a route_list <<<"$MATMUL_FULL_FACTORIZED_ROUTES"
  for entry in "${route_list[@]}"; do
    if [[ "$entry" == "$route" || "$entry" == "all" ]]; then
      return 0
    fi
  done
  return 1
}

size_tag() {
  echo "$1" | tr 'x' '_'
}

size_descriptor() {
  local m0="$1"
  local m1="$2"
  local n0="$3"
  local n1="$4"
  local k0="$5"
  local k1="$6"
  echo "m0=$m0;m1=$m1;m=$((m0 * m1));n0=$n0;n1=$n1;n=$((n0 * n1));k0=$k0;k1=$k1;k=$((k0 * k1))"
}

metric_field_local() {
  local key="$1"
  local file="$2"
  if [[ ! -f "$file" ]]; then
    echo "NA"
    return
  fi
  awk -F= -v key="$key" '$1 == key { print $2; found=1; exit } END { if (!found) print "NA" }' "$file"
}

count_pattern() {
  local pattern="$1"
  local file="$2"
  if [[ ! -f "$file" ]]; then
    echo "0"
    return
  fi
  (rg -o "$pattern" "$file" || true) | wc -l | tr -d ' '
}

tail_bound_kind() {
  local file="$1"
  if rg -q ' to min |affine\.min' "$file"; then
    echo "affine_min"
  elif rg -q 'arith\.minsi' "$file"; then
    echo "arith_minsi"
  elif rg -q 'd_affine\.min' "$file"; then
    echo "d_affine_min"
  else
    echo "none"
  fi
}

require_pattern() {
  local file="$1"
  local pattern="$2"
  local message="$3"
  if ! rg -q "$pattern" "$file"; then
    echo "error: $message: $file" >&2
    exit 1
  fi
}

require_no_tail() {
  local file="$1"
  local message="$2"
  if [[ "$(tail_bound_kind "$file")" != "none" ]]; then
    echo "error: $message: $file" >&2
    exit 1
  fi
}

compile_executable() {
  local variant="$1"
  local driver="$2"
  local llvm_ir="$3"
  local prefix="$4"
  local m0="$5"
  local m1="$6"
  local n0="$7"
  local n1="$8"
  local k0="$9"
  local k1="${10}"
  local obj="$prefix.o"
  local exe="$prefix.exec"
  "$CC" -O2 -x ir "$llvm_ir" -c -o "$obj"
  "$CC" -O2 \
    -DBENCH_LABEL="\"$BENCHMARK_NAME\"" \
    -DVARIANT_LABEL="\"$variant\"" \
    -DMATMUL_FULL_FACTORIZED_M0="$m0" \
    -DMATMUL_FULL_FACTORIZED_M1="$m1" \
    -DMATMUL_FULL_FACTORIZED_N0="$n0" \
    -DMATMUL_FULL_FACTORIZED_N1="$n1" \
    -DMATMUL_FULL_FACTORIZED_K0="$k0" \
    -DMATMUL_FULL_FACTORIZED_K1="$k1" \
    "$driver" "$obj" -o "$exe"
}

build_mlir_route() {
  local variant="$1"
  local tag="$2"
  local m0="$3"
  local m1="$4"
  local n0="$5"
  local n1="$6"
  local k0="$7"
  local k1="$8"
  local prefix="$OUT_DIR/${tag}_${variant}"
  cp "$MLIR_SRC" "$prefix.input.mlir"
  local start_ns
  local end_ns
  start_ns="$(now_ns)"
  "$MLIR_OPT" "$MLIR_SRC" --affine-loop-tile=tile-size="$MATMUL_FULL_FACTORIZED_TILE_SIZE" > "$prefix.tiled.mlir"
  "$MLIR_OPT" "$MLIR_SRC" \
    --affine-loop-tile=tile-size="$MATMUL_FULL_FACTORIZED_TILE_SIZE" \
    --lower-affine \
    --convert-scf-to-cf \
    --expand-strided-metadata \
    --finalize-memref-to-llvm \
    --convert-arith-to-llvm \
    --convert-index-to-llvm \
    --convert-cf-to-llvm \
    --convert-func-to-llvm \
    --reconcile-unrealized-casts \
    > "$prefix.llvm.mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$prefix.llvm.mlir" > "$prefix.ll"
  compile_executable "$variant" "$MLIR_DRIVER" "$prefix.ll" "$prefix" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
  end_ns="$(now_ns)"
  run_benchmark_repeated "$prefix.output.txt" "$prefix.exec" "$ITERATIONS"
  printf 'build_status=ok\ncompile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")" >> "$prefix.output.txt"
}

build_scair_route() {
  local variant="$1"
  local src="$2"
  local driver="$3"
  local pre_lower="$4"
  local lower="$5"
  local tag="$6"
  local m0="$7"
  local m1="$8"
  local n0="$9"
  local n1="${10}"
  local k0="${11}"
  local k1="${12}"
  local prefix="$OUT_DIR/${tag}_${variant}"
  cp "$src" "$prefix.input.mlir"
  local start_ns
  local end_ns
  start_ns="$(now_ns)"
  run_scair_opt -s "$src" --passes "$pre_lower" > "$prefix.tiled.mlir"
  run_scair_opt -s "$src" --passes "$lower,convert-func-to-llvm,convert-llvm-export-abi" > "$prefix.llvm.mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$prefix.llvm.mlir" > "$prefix.ll"
  compile_executable "$variant" "$driver" "$prefix.ll" "$prefix" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
  end_ns="$(now_ns)"
  run_benchmark_repeated "$prefix.output.txt" "$prefix.exec" "$ITERATIONS"
  printf 'build_status=ok\ncompile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")" >> "$prefix.output.txt"
}

build_scair_to_mlir_lower_route() {
  local variant="$1"
  local src="$2"
  local pre_lower="$3"
  local tag="$4"
  local m0="$5"
  local m1="$6"
  local n0="$7"
  local n1="$8"
  local k0="$9"
  local k1="${10}"
  local prefix="$OUT_DIR/${tag}_${variant}"
  cp "$src" "$prefix.input.mlir"
  local start_ns
  local end_ns
  start_ns="$(now_ns)"
  run_scair_opt -s "$src" --passes "$pre_lower" > "$prefix.tiled.mlir"
  "$MLIR_OPT" "$prefix.tiled.mlir" \
    --lower-affine \
    --convert-scf-to-cf \
    --expand-strided-metadata \
    --finalize-memref-to-llvm \
    --convert-arith-to-llvm \
    --convert-index-to-llvm \
    --convert-cf-to-llvm \
    --convert-func-to-llvm \
    --reconcile-unrealized-casts \
    > "$prefix.llvm.mlir"
  "$MLIR_TRANSLATE" --mlir-to-llvmir "$prefix.llvm.mlir" > "$prefix.ll"
  compile_executable "$variant" "$MLIR_DRIVER" "$prefix.ll" "$prefix" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
  end_ns="$(now_ns)"
  run_benchmark_repeated "$prefix.output.txt" "$prefix.exec" "$ITERATIONS"
  printf 'build_status=ok\ncompile_ms=%s\n' "$(format_ms "$start_ns" "$end_ns")" >> "$prefix.output.txt"
}

SUMMARY_MD="$OUT_DIR/summary.md"
METRICS_CSV="$OUT_DIR/metrics.csv"
METRICS_JSON="$OUT_DIR/metrics.json"

cat > "$SUMMARY_MD" <<'MD'
# Matmul Full Factorized Tiling Benchmark Summary

| Variant | Size | Build | Run | Tail bound | Dynamic steps | affine.for | d_affine.for | MLIR LOC | LLVM LOC | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
MD

cat > "$METRICS_CSV" <<'CSV'
benchmark,variant,size,build_status,run_status,input,tiled,llvm_mlir,llvm_ir,tile_loop,shared_tile_size,product_representation,tail_bound,same_loop_target,mlir_baseline,affine_min_count,arith_minsi_count,d_affine_for_count,affine_for_count,dynamic_step_count,mlir_loc,llvm_loc,result,expected_result,ns_per_iter,compile_ms,notes
CSV

append_case() {
  local variant="$1"
  local tag="$2"
  local size="$3"
  local product_representation="$4"
  local mlir_baseline="$5"
  local notes="$6"
  local prefix="$OUT_DIR/${tag}_${variant}"
  local tail
  tail="$(tail_bound_kind "$prefix.tiled.mlir")"
  local dynamic_steps
  dynamic_steps="$(count_pattern 'step %[A-Za-z0-9_]+' "$prefix.tiled.mlir")"
  local affine_for
  affine_for="$(count_pattern '(^|[^_])affine\.for' "$prefix.tiled.mlir")"
  local d_affine_for
  d_affine_for="$(count_pattern 'd_affine\.for' "$prefix.tiled.mlir")"
  local affine_min
  affine_min="$(count_pattern 'affine\.min| to min ' "$prefix.tiled.mlir")"
  local arith_minsi
  arith_minsi="$(count_pattern 'arith\.minsi' "$prefix.tiled.mlir")"
  local mlir_loc
  mlir_loc="$(file_metric lines "$prefix.llvm.mlir")"
  local llvm_loc
  llvm_loc="$(file_metric lines "$prefix.ll")"
  local result
  result="$(metric_field_local result "$prefix.output.txt")"
  local expected
  expected="$(metric_field_local expected_result "$prefix.output.txt")"
  local ns
  ns="$(metric_field_local ns_per_iter "$prefix.output.txt")"
  local build
  build="$(metric_field_local build_status "$prefix.output.txt")"
  local run
  run="$(metric_field_local run_status "$prefix.output.txt")"
  local compile
  compile="$(metric_field_local compile_ms "$prefix.output.txt")"

  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "$BENCHMARK_NAME" "$variant" "$size" "$build" "$run" \
    "$prefix.input.mlir" "$prefix.tiled.mlir" "$prefix.llvm.mlir" "$prefix.ll" \
    "i+j+p" "$MATMUL_FULL_FACTORIZED_TILE_SIZE" "$product_representation" "$tail" "yes" "$mlir_baseline" \
    "$affine_min" "$arith_minsi" "$d_affine_for" "$affine_for" "$dynamic_steps" \
    "$mlir_loc" "$llvm_loc" "$result" "$expected" "$ns" "$compile" "$notes" >> "$METRICS_CSV"

  printf '| `%s` | `%s` | %s | %s | `%s` | %s | %s | %s | %s | %s | %s | %s | %s |\n' \
    "$variant" "$size" "$build" "$run" "$tail" "$dynamic_steps" "$affine_for" "$d_affine_for" \
    "$mlir_loc" "$llvm_loc" "$result" "$expected" "$ns" >> "$SUMMARY_MD"
}

IFS=',' read -r -a sizes <<<"$MATMUL_FULL_FACTORIZED_TILING_SIZE_SET"
for dims in "${sizes[@]}"; do
  IFS='x' read -r m0 m1 n0 n1 k0 k1 <<<"$dims"
  if [[ -z "${m0:-}" || -z "${m1:-}" || -z "${n0:-}" || -z "${n1:-}" || -z "${k0:-}" || -z "${k1:-}" ]]; then
    echo "error: invalid MATMUL_FULL_FACTORIZED_TILING_SIZE_SET entry '$dims' (expected M0xM1xN0xN1xK0xK1)" >&2
    exit 1
  fi
  tag="$(size_tag "$dims")"
  size="$(size_descriptor "$m0" "$m1" "$n0" "$n1" "$k0" "$k1")"

  if route_enabled "mlir_baseline_full_tile"; then
    echo "==> Building MLIR baseline full tile for $size"
    build_mlir_route "mlir_baseline_full_tile" "$tag" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
    require_pattern "$OUT_DIR/${tag}_mlir_baseline_full_tile.tiled.mlir" 'step 64' "MLIR baseline must tile with step 64"
    append_case "mlir_baseline_full_tile" "$tag" "$size" "arith.muli_index" "yes" "upstream_mlir_affine_loop_tile_outer_legal_bands"
  fi

  if route_enabled "ordinary_scair_full_tile_with_tail"; then
    echo "==> Building ordinary ScaIR full tile with tail for $size"
    build_scair_to_mlir_lower_route \
      "ordinary_scair_full_tile_with_tail" \
      "$ORDINARY_SRC" \
      "canonicalize,cse,dce,ordinary-affine-context-band-tile-with-tail:$MATMUL_FULL_FACTORIZED_TILE_SIZE,ordinary-affine-product-loop-tile-with-tail:$k1,canonicalize,cse,dce" \
      "$tag" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
    require_pattern "$OUT_DIR/${tag}_ordinary_scair_full_tile_with_tail.tiled.mlir" 'step 64' "ordinary ScaIR must tile with step 64"
    require_pattern "$OUT_DIR/${tag}_ordinary_scair_full_tile_with_tail.tiled.mlir" ' to min ' "ordinary ScaIR must keep guarded min tails"
    append_case "ordinary_scair_full_tile_with_tail" "$tag" "$size" "arith.muli_index" "no" "ordinary_scair_control_same_i_j_p_loop_target"
  fi

  if route_enabled "dependent_full_guarded_tail_simplified"; then
    echo "==> Building dependent guarded-then-simplified full tile for $size"
    guarded="$OUT_DIR/${tag}_dependent_full_guarded_tail_simplified.guarded.mlir"
    run_scair_opt -s "$DEPENDENT_SRC" --passes "canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-factor-tile-with-tail,dependent-tile-with-tail-control,validate-d-affine-dynamic-steps,canonicalize,cse,dce" > "$guarded"
    require_pattern "$guarded" 'arith\.minsi' "dependent guarded artifact must contain dynamic tail min"
    build_scair_route \
      "dependent_full_guarded_tail_simplified" \
      "$DEPENDENT_SRC" \
      "$DEPENDENT_DRIVER" \
      "canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-factor-tile-with-tail,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce" \
      "canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-factor-tile-with-tail,dependent-tile-with-tail-control,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,canonicalize,cse,dce,lower-d-memref-to-llvm" \
      "$tag" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
    require_no_tail "$OUT_DIR/${tag}_dependent_full_guarded_tail_simplified.tiled.mlir" "dependent guarded simplified route must remove tail guards"
    append_case "dependent_full_guarded_tail_simplified" "$tag" "$size" "d_tensor.nat.mul" "no" "guarded_artifact=$(basename "$guarded");proof_removes_i_j_p_tail"
  fi

  if route_enabled "dependent_full_exact_tile"; then
    echo "==> Building dependent exact full tile for $size"
    build_scair_route \
      "dependent_full_exact_tile" \
      "$DEPENDENT_SRC" \
      "$DEPENDENT_DRIVER" \
      "canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-exact-tile,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce" \
      "canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-exact-tile,dependent-product-loop-exact-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce,lower-d-memref-to-llvm" \
      "$tag" "$m0" "$m1" "$n0" "$n1" "$k0" "$k1"
    require_no_tail "$OUT_DIR/${tag}_dependent_full_exact_tile.tiled.mlir" "dependent exact route must be tail-free"
    append_case "dependent_full_exact_tile" "$tag" "$size" "d_tensor.nat.mul" "no" "diagnostic_direct_exact_i_j_p_tiling"
  fi
done

python3 - "$METRICS_CSV" "$METRICS_JSON" <<'PY'
import csv
import json
import sys

with open(sys.argv[1], newline="", encoding="utf-8") as f:
    rows = list(csv.DictReader(f))
with open(sys.argv[2], "w", encoding="utf-8") as f:
    json.dump(rows, f, indent=2)
PY

cat >> "$SUMMARY_MD" <<MD

## Interpretation

This benchmark is the composition check for factorized matmul tiling. It combines
the output-space facts \`M=M0*M1\` and \`N=N0*N1\` with the reduction fact
\`K=K0*K1\`, then tiles \`i\`, \`j\`, and \`p\` in one benchmark family. The
dependent guarded route demonstrates that the product proofs compose strongly
enough to remove the emitted tail guards. This is structural evidence, not a
standalone parallel speedup claim, because reduction tiling still needs a
separate partial-sum lowering for parallel execution.

Environment: \`$ENV_PATH\`
Git commit: \`$GIT_COMMIT\`
Run date: \`$RUN_DATE\`
Machine: \`$MACHINE_ID\`
MD

echo
echo "Matmul full-factorized tiling benchmark complete."
echo "Produced:"
echo "  $OUT_DIR/metrics.csv"
echo "  $OUT_DIR/metrics.json"
echo "  $OUT_DIR/summary.md"
