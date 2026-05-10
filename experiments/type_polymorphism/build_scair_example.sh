#!/usr/bin/env bash
set -euo pipefail

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCAIR_ROOT/experiments/common_metrics.sh"

SCAIR_OPT="${SCAIR_OPT:-$SCAIR_ROOT/out/tools/opt/launcher.dest/run}"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/out}"

mkdir -p "$OUT_DIR"
ENV_PATH="$(ensure_env_snapshot "$OUT_DIR")"
GIT_COMMIT="$(git_commit_for_metrics)"
RUN_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
MACHINE_ID="$(machine_id_for_metrics)"

require_bin "$SCAIR_OPT"

mono_pipeline_for_variant() {
  case "$1" in
    debruijn) echo "monomorphize-tlam-de-bruijn" ;;
    value_dependent) echo "monomorphize" ;;
    *)
      echo "error: unknown variant: $1" >&2
      exit 1
      ;;
  esac
}

full_pipeline_for_variant() {
  case "$1" in
    debruijn)
      echo "monomorphize-tlam-de-bruijn,beta-reduce-tlam-de-bruijn,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,reconcile-unrealized-casts,canonicalize"
      ;;
    value_dependent)
      echo "monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize"
      ;;
    *)
      echo "error: unknown variant: $1" >&2
      exit 1
      ;;
  esac
}

representation_group_for_variant() {
  case "$1" in
    debruijn) echo "scair_baseline" ;;
    value_dependent) echo "value_dependent" ;;
    *)
      echo "error: unknown variant: $1" >&2
      exit 1
      ;;
  esac
}

source_for_case() {
  local bench="$1"
  local variant="$2"
  case "$bench:$variant" in
    polymorphic_identity_specialization:debruijn)
      echo "$EXAMPLE_DIR/baseline_de_bruijn_polymorphic_identity_specialization.mlir"
      ;;
    polymorphic_identity_specialization:value_dependent)
      echo "$EXAMPLE_DIR/value_dependent_polymorphic_identity_specialization.mlir"
      ;;
    tensor_shape_identity:value_dependent)
      echo "$EXAMPLE_DIR/value_dependent_tensor_shape_identity.mlir"
      ;;
    *)
      echo "error: unknown benchmark/variant: $bench/$variant" >&2
      exit 1
      ;;
  esac
}

case_notes() {
  local bench="$1"
  local variant="$2"
  case "$bench:$variant" in
    polymorphic_identity_specialization:debruijn)
      echo "de_bruijn_identity_two_specializations"
      ;;
    polymorphic_identity_specialization:value_dependent)
      echo "ssa_in_types_identity_two_specializations"
      ;;
    tensor_shape_identity:value_dependent)
      echo "tensor_shaped_type_argument_single_specialization"
      ;;
    *)
      echo "NA"
      ;;
  esac
}

count_leftover_polymorphic_ops() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  count_matches '"tlam(_dbi)?\.(tlambda|tapply|treturn)"|!tlam(_dbi)?\.(forall|bvar)|!value<%' "$path"
}

count_leftover_tlam_ops() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  count_matches '"tlam(_dbi)?\.[A-Za-z_]+"' "$path"
}

count_generated_specializations() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  count_matches '"tlam(_dbi)?\.vlambda"' "$path"
}

run_pipeline() {
  local src="$1"
  local passes="$2"
  local out="$3"
  local log="$4"
  local start_ns="$5"
  local status=0

  set +e
  "$SCAIR_OPT" "$src" --allow-unregistered-dialect --passes "$passes" \
    > "$out" 2> "$log"
  status=$?
  set -e

  if [[ $status -ne 0 ]]; then
    printf 'pass_status=fail\n' >> "$log"
  fi
  printf '%s\n' "$status"
}

write_design_header() {
  local path="$1"
  printf '%s\n' "benchmark,variant,input_op_count,output_op_count,generated_specializations,leftover_polymorphic_ops,leftover_tlam_ops,input_ir_lines,output_ir_lines,pass_status,compile_ms,artifact,notes" > "$path"
}

append_design_row() {
  local path="$1"
  shift
  local sanitized=()
  local value
  for value in "$@"; do
    value="${value//$'\n'/ }"
    value="${value//$'\r'/ }"
    value="${value//,/;}"
    sanitized+=("$value")
  done
  printf '%s\n' "$(IFS=,; echo "${sanitized[*]}")" >> "$path"
}

append_common_row() {
  local metrics_csv="$1"
  local summary_md="$2"
  local bench="$3"
  local variant="$4"
  local src="$5"
  local final_ir="$6"
  local compile_ms="$7"
  local pass_status="$8"
  local generated_specializations="$9"
  local leftover_poly="${10}"
  local leftover_tlam="${11}"
  local notes="${12}"
  local representation
  representation="$(representation_group_for_variant "$variant")"

  append_metrics_csv_row \
    "$metrics_csv" \
    "type_polymorphism" \
    "$bench" \
    "$variant" \
    "$representation" \
    "$pass_status" \
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
    "$(count_source_dmemref_load_ops "$src")" \
    "$(count_source_dmemref_store_ops "$src")" \
    "$(count_func_defs "$final_ir")" \
    "$(count_ops "$final_ir")" \
    "$(count_ops_structural "$final_ir")" \
    "$(file_metric lines "$final_ir")" \
    "NA" \
    "NA" \
    "$compile_ms" \
    "NA" \
    "NA" \
    "NA" \
    "$notes;specializations=$generated_specializations;leftover_polymorphic_ops=$leftover_poly;leftover_tlam_ops=$leftover_tlam" \
    "$(count_source_helpers "$src")" \
    "$(count_matches 'bvar<' "$src")" \
    "$(count_matches 'value<%' "$src")" \
    "NA" \
    "NA" \
    "type_polymorphism" \
    "benchmark=${bench};role=design_infrastructure" \
    "$variant" \
    "NA" \
    "NA" \
    "NA" \
    "$compile_ms" \
    "NA" \
    "NA" \
    "NA" \
    "NA" \
    "$pass_status" \
    "NA" \
    "$GIT_COMMIT" \
    "$RUN_DATE" \
    "$MACHINE_ID" \
    "$ENV_PATH" \
    "NA"

  append_summary_row \
    "$summary_md" \
    "$bench" \
    "$variant" \
    "$representation" \
    "$pass_status" \
    "NA" \
    "$(count_ops_structural "$src")" \
    "$(count_func_defs "$src")" \
    "$(count_block_args "$src")" \
    "$(file_metric lines "$final_ir")" \
    "NA" \
    "$compile_ms" \
    "NA" \
    "NA" \
    "NA"
}

CASES=(
  "polymorphic_identity_specialization|value_dependent"
  "polymorphic_identity_specialization|debruijn"
  "tensor_shape_identity|value_dependent"
)

SUMMARY_MD="$OUT_DIR/summary.md"
SUMMARY_CSV="$OUT_DIR/metrics.csv"
DESIGN_CSV="$OUT_DIR/design_metrics.csv"
METRICS_JSON="$OUT_DIR/metrics.json"

write_summary_header "$SUMMARY_MD" "Type Polymorphism Design Benchmark Summary"
write_metrics_csv_header "$SUMMARY_CSV"
write_design_header "$DESIGN_CSV"

for case_spec in "${CASES[@]}"; do
  IFS='|' read -r bench variant <<< "$case_spec"
  src="$(source_for_case "$bench" "$variant")"
  require_file "$src"

  prefix="${bench}_${variant}"
  mono_ir="$OUT_DIR/${prefix}.monomorphized.mlir"
  final_ir="$OUT_DIR/${prefix}.erased_lowered.mlir"
  mono_log="$OUT_DIR/${prefix}.monomorphized.log"
  final_log="$OUT_DIR/${prefix}.erased_lowered.log"

  echo "==> Running $bench ($variant)"
  start_ns="$(now_ns)"
  mono_status="$(run_pipeline "$src" "$(mono_pipeline_for_variant "$variant")" "$mono_ir" "$mono_log" "$start_ns")"
  final_status=1
  if [[ "$mono_status" == "0" ]]; then
    final_status="$(run_pipeline "$src" "$(full_pipeline_for_variant "$variant")" "$final_ir" "$final_log" "$start_ns")"
  else
    : > "$final_ir"
  fi
  end_ns="$(now_ns)"
  compile_ms="$(format_ms "$start_ns" "$end_ns")"

  if [[ "$mono_status" == "0" && "$final_status" == "0" ]]; then
    pass_status="ok"
  else
    pass_status="fail"
  fi

  generated_specializations="$(count_generated_specializations "$mono_ir")"
  leftover_poly="$(count_leftover_polymorphic_ops "$final_ir")"
  leftover_tlam="$(count_leftover_tlam_ops "$final_ir")"
  notes="$(case_notes "$bench" "$variant")"

  append_design_row \
    "$DESIGN_CSV" \
    "$bench" \
    "$variant" \
    "$(count_ops_structural "$src")" \
    "$(count_ops_structural "$final_ir")" \
    "$generated_specializations" \
    "$leftover_poly" \
    "$leftover_tlam" \
    "$(file_metric lines "$src")" \
    "$(file_metric lines "$final_ir")" \
    "$pass_status" \
    "$compile_ms" \
    "$final_ir" \
    "$notes"

  append_common_row \
    "$SUMMARY_CSV" \
    "$SUMMARY_MD" \
    "$bench" \
    "$variant" \
    "$src" \
    "$final_ir" \
    "$compile_ms" \
    "$pass_status" \
    "$generated_specializations" \
    "$leftover_poly" \
    "$leftover_tlam" \
    "$notes"
done

python3 - "$DESIGN_CSV" "$METRICS_JSON" <<'PY'
import csv
import json
import sys

csv_path, json_path = sys.argv[1], sys.argv[2]
rows = []
with open(csv_path, newline="", encoding="utf-8") as f:
    for row in csv.DictReader(f):
        rows.append(row)

payload = {
    "benchmark_family": "type_polymorphism",
    "benchmark_role": "design_infrastructure",
    "runtime_performance_benchmark": False,
    "rows": rows,
}

with open(json_path, "w", encoding="utf-8") as f:
    json.dump(payload, f, indent=2, sort_keys=True)
    f.write("\n")
PY

cat >> "$SUMMARY_MD" <<'EOF'

## Design Metrics

This is a design/infrastructure benchmark. The thesis-facing checks are the
generated specialization count, the absence of leftover polymorphic/type-level
TLam constructs after erasure/lowering, and small IR size. It is not a central
runtime or performance benchmark.
EOF

echo
echo "Type polymorphism design benchmark complete."
echo "Produced:"
echo "  $SUMMARY_MD"
echo "  $SUMMARY_CSV"
echo "  $DESIGN_CSV"
echo "  $METRICS_JSON"
