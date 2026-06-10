#!/usr/bin/env bash

# Shared experiment metric schema.
#
# Core columns are intended to be stable across all benchmark families so the
# per-family CSVs can be concatenated directly by experiments/build_all_metrics.sh.
# Some columns are most meaningful for particular families:
# - design benchmarks: source_ops_structural, source_func_defs,
#   source_block_args, source_helper_defs, bvar_refs, value_ssa_refs
# - memref/control-flow benchmarks: source_alloc_ops, source_reinterpret_cast_ops,
#   source_subview_ops, source_extract_strided_metadata_ops, memref/d_memref
#   load/store counts
# - kernel benchmarks: lowered_mlir_lines, llvm_ir_lines, llvm_call_count, compile_ms,
#   runtime_ns_per_iter
#
# Metrics that do not apply to a family should emit 0 when the operation kind is
# absent from the source, and NA only when the source/artifact itself does not
# exist or the metric is fundamentally unavailable.
#
# `source_ops` is kept as a lightweight textual estimate for backward
# compatibility. `source_ops_structural` and `lowered_ops_structural` are the
# preferred thesis-facing metrics: they come from `scair-opt --emit-ir-metrics`,
# which traverses parsed IR and prints deterministic `key=value` fields such as:
#   total_ops=...
#   func_defs=...
#   blocks=...
#   block_args=...
#   op.memref.alloc=...
COMMON_METRICS_HEADER="experiment_family,benchmark,variant,representation_group,build_status,run_status,source_bytes,source_loc,source_ops,source_ops_structural,source_func_defs,source_block_args,source_alloc_ops,source_reinterpret_cast_ops,source_subview_ops,source_extract_strided_metadata_ops,source_memref_load_ops,source_memref_store_ops,source_d_memref_load_ops,source_d_memref_store_ops,lowered_func_defs,lowered_ops,lowered_ops_structural,lowered_mlir_lines,llvm_ir_lines,llvm_call_count,compile_ms,result,expected_result,runtime_ns_per_iter,notes,source_helper_defs,bvar_refs,value_ssa_refs,opt_llvm_lines,opt_llvm_call_count,kernel,size,route,parse_time_ms,verification_time_ms,lowering_time_ms,compile_total_ms,runtime_median_ns_per_iter,runtime_iqr_ns_per_iter,benchmark_repetitions,checksum,checksum_status,compiler_flags,git_commit,date,machine_id,env_path,raw_timings_path"

if [[ "${TILING_BENCHMARK_QUICK:-0}" == "1" ]]; then
  BENCH_WARMUP_REPS="${BENCH_WARMUP_REPS:-1}"
  BENCH_TIMING_REPS="${BENCH_TIMING_REPS:-1}"
  TILING_BENCHMARK_MAX_SHAPES="${TILING_BENCHMARK_MAX_SHAPES:-1}"
else
  BENCH_WARMUP_REPS="${BENCH_WARMUP_REPS:-5}"
  BENCH_TIMING_REPS="${BENCH_TIMING_REPS:-15}"
fi
BENCH_PROGRESS="${BENCH_PROGRESS:-1}"
TILING_BENCHMARK_MAX_SHAPES="${TILING_BENCHMARK_MAX_SHAPES:-0}"

require_file() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "error: missing file: $path" >&2
    exit 1
  fi
}

require_bin() {
  local path="$1"
  if [[ ! -x "$path" ]]; then
    echo "error: missing executable: $path" >&2
    exit 1
  fi
}

limit_csv_entries() {
  local csv="$1"
  local limit="${2:-$TILING_BENCHMARK_MAX_SHAPES}"
  if [[ -z "$csv" ]]; then
    printf '\n'
    return
  fi
  if [[ -z "$limit" || "$limit" == "0" ]]; then
    printf '%s\n' "$csv"
    return
  fi
  awk -v limit="$limit" 'BEGIN { RS=","; ORS=""; count=0 } count < limit { if (count > 0) printf ","; printf "%s", $0; count++ }' <<<"$csv"
}

try_lower_d_memref_to_llvm_artifacts() {
  local input_mlir="$1"
  local llvm_mlir="$2"
  local llvm_ir="$3"
  local status_path="$4"
  local scair_opt="${SCAIR_OPT:-}"
  local mlir_translate="${MLIR_TRANSLATE:-}"

  if [[ -z "$scair_opt" || ! -x "$scair_opt" ]]; then
    echo "status=unsupported" > "$status_path"
    echo "reason=missing_scair_opt" >> "$status_path"
    return 0
  fi

  if "$scair_opt" -s "$input_mlir" --passes "canonicalize,cse,dce,lower-d-memref-to-llvm" > "$llvm_mlir" 2> "${status_path}.log"; then
    if [[ -n "$mlir_translate" && -x "$mlir_translate" ]] &&
       "$mlir_translate" --mlir-to-llvmir "$llvm_mlir" > "$llvm_ir" 2>> "${status_path}.log"; then
      echo "status=ok" > "$status_path"
      echo "llvm_mlir=$(basename "$llvm_mlir")" >> "$status_path"
      echo "llvm_ir=$(basename "$llvm_ir")" >> "$status_path"
    else
      echo "status=llvm_mlir_only" > "$status_path"
      echo "llvm_mlir=$(basename "$llvm_mlir")" >> "$status_path"
      echo "reason=mlir_translate_failed_or_missing" >> "$status_path"
    fi
  else
    rm -f "$llvm_mlir" "$llvm_ir"
    echo "status=unsupported" > "$status_path"
    echo "reason=lower_d_memref_to_llvm_failed" >> "$status_path"
  fi
}

require_nonempty_file() {
  local path="$1"
  if [[ ! -s "$path" ]]; then
    echo "error: missing or empty file: $path" >&2
    exit 1
  fi
}

header_column_count() {
  awk -F, 'NR == 1 { print NF }' <<<"$COMMON_METRICS_HEADER"
}

now_ns() {
  date +%s%N
}

format_ms() {
  local start_ns="$1"
  local end_ns="$2"
  awk -v start="$start_ns" -v end="$end_ns" 'BEGIN { printf "%.2f", (end - start) / 1000000.0 }'
}

file_metric() {
  local mode="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  case "$mode" in
    bytes)
      wc -c < "$path" | tr -d ' '
      ;;
    lines)
      wc -l < "$path" | tr -d ' '
      ;;
    *)
      echo "NA"
      ;;
  esac
}

count_matches() {
  local pattern="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count=$(rg -o "$pattern" "$path" 2>/dev/null | wc -l | tr -d ' ')
  echo "${count:-0}"
}

count_ops() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count=$(
    rg '^[[:space:]]*(%[^=[:space:]]+[[:space:]]*=.*|call @|return([[:space:]]|$)|func\.return|llvm\.call|func\.call|"[^"]+")' "$path" \
      | wc -l | tr -d ' '
  )
  echo "${count:-0}"
}

scair_opt_for_metrics() {
  if [[ -n "${SCAIR_OPT:-}" && -x "${SCAIR_OPT:-}" ]]; then
    echo "$SCAIR_OPT"
    return
  fi

  local root="${SCAIR_ROOT:-}"
  local candidate
  if [[ -n "$root" ]]; then
    candidate="$root/out/tools/opt/launcher.dest/run"
    if [[ -x "$candidate" ]]; then
      echo "$candidate"
      return
    fi
  fi

  echo ""
}

mlir_opt_for_metrics() {
  if [[ -n "${MLIR_OPT:-}" && -x "${MLIR_OPT:-}" ]]; then
    echo "$MLIR_OPT"
    return
  fi

  local llvm_build="${LLVM_BUILD_DIR:-$HOME/dev/llvm-clean-build}"
  local candidate="$llvm_build/bin/mlir-opt"
  if [[ -x "$candidate" ]]; then
    echo "$candidate"
    return
  fi

  echo ""
}

emit_ir_metrics_for_file() {
  local path="$1"
  local out="$2"
  local scair_opt
  local mlir_opt
  local generic_tmp
  local stats_tmp

  scair_opt="$(scair_opt_for_metrics)"
  if [[ -n "$scair_opt" ]]; then
    if "$scair_opt" -a -s --emit-ir-metrics "$path" > "$out" 2>/dev/null; then
      return 0
    fi
  fi

  mlir_opt="$(mlir_opt_for_metrics)"
  if [[ -n "$mlir_opt" ]]; then
    generic_tmp="$(mktemp)"
    stats_tmp="$(mktemp)"

    if [[ -n "$scair_opt" ]] && "$mlir_opt" --allow-unregistered-dialect --mlir-print-op-generic "$path" > "$generic_tmp" 2>/dev/null; then
      if "$scair_opt" -a -s --emit-ir-metrics "$generic_tmp" > "$out" 2>/dev/null; then
        rm -f "$generic_tmp" "$stats_tmp"
        return 0
      fi
    fi

    if "$mlir_opt" --allow-unregistered-dialect --mlir-print-op-generic "$path" > "$generic_tmp" 2>/dev/null \
      && "$mlir_opt" --allow-unregistered-dialect --print-op-stats "$path" > "$stats_tmp" 2>&1; then
      python3 - "$generic_tmp" "$stats_tmp" > "$out" <<'PY'
import re
import sys

generic_path, stats_path = sys.argv[1], sys.argv[2]
tracked = [
    "builtin.module",
    "func.func",
    "llvm.func",
    "memref.alloc",
    "d_memref.alloc",
    "memref.reinterpret_cast",
    "d_memref.reinterpret_cast",
    "memref.subview",
    "d_memref.subview",
    "memref.extract_strided_metadata",
    "d_memref.extract_strided_metadata",
    "memref.load",
    "memref.store",
    "d_memref.load",
    "d_memref.store",
]

op_counts = {}
in_table = False
for line in open(stats_path, "r", encoding="utf-8"):
    if line.startswith("Operations encountered:"):
        in_table = True
        continue
    if not in_table:
        continue
    match = re.match(r"^\s*([A-Za-z0-9_.]+)\s*,\s*([0-9]+)\s*$", line)
    if match:
        op_counts[match.group(1)] = int(match.group(2))

block_count = 0
block_args = 0
for line in open(generic_path, "r", encoding="utf-8"):
    match = re.match(r"^\s*\^bb[^(]*\(([^)]*)\)", line)
    if not match:
        continue
    block_count += 1
    body = match.group(1).strip()
    if body:
        block_args += len([part for part in body.split(",") if part.strip()])

all_ops = sorted(set(tracked) | set(op_counts))
print("status=ok")
print(f"total_ops={sum(op_counts.values())}")
print(f"func_defs={op_counts.get('func.func', 0) + op_counts.get('llvm.func', 0)}")
print(f"blocks={block_count}")
print(f"block_args={block_args}")
for name in all_ops:
    print(f"op.{name}={op_counts.get(name, 0)}")
PY
      rm -f "$generic_tmp" "$stats_tmp"
      return 0
    fi

    rm -f "$generic_tmp" "$stats_tmp"
  fi

  return 1
}

ensure_ir_metrics_cache() {
  if ! declare -p SCAIR_IR_METRICS_CACHE >/dev/null 2>&1; then
    declare -gA SCAIR_IR_METRICS_CACHE=()
  fi
}

ir_metrics_file() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo ""
    return
  fi

  ensure_ir_metrics_cache

  if [[ -n "${SCAIR_IR_METRICS_CACHE[$path]:-}" && -f "${SCAIR_IR_METRICS_CACHE[$path]}" ]]; then
    echo "${SCAIR_IR_METRICS_CACHE[$path]}"
    return
  fi

  local tmp
  tmp="$(mktemp)"
  if emit_ir_metrics_for_file "$path" "$tmp"; then
    SCAIR_IR_METRICS_CACHE["$path"]="$tmp"
  else
    printf 'status=NA\n' > "$tmp"
    SCAIR_IR_METRICS_CACHE["$path"]="$tmp"
  fi

  echo "$tmp"
}

ir_metric_field() {
  local path="$1"
  local key="$2"
  local default_value="${3:-0}"
  local metrics_path

  metrics_path="$(ir_metrics_file "$path")"
  if [[ -z "$metrics_path" || ! -f "$metrics_path" ]]; then
    echo "NA"
    return
  fi

  if [[ "$(metric_field status "$metrics_path")" == "NA" ]]; then
    echo "NA"
    return
  fi

  local value
  value="$(metric_field "$key" "$metrics_path")"
  if [[ -z "$value" ]]; then
    echo "$default_value"
  else
    echo "$value"
  fi
}

count_ops_structural() {
  ir_metric_field "$1" "total_ops" "0"
}

count_func_defs() {
  ir_metric_field "$1" "func_defs" "0"
}

count_source_helpers() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local funcs
  funcs=$(count_func_defs "$path")
  if [[ "$funcs" == "NA" || "$funcs" -le 1 ]]; then
    echo "0"
  else
    echo $((funcs - 1))
  fi
}

count_block_args() {
  ir_metric_field "$1" "block_args" "0"
}

count_alloc_ops() {
  sum_numeric_or_na \
    "$(ir_metric_field "$1" "op.memref.alloc" "0")" \
    "$(ir_metric_field "$1" "op.d_memref.alloc" "0")"
}

count_source_reinterpret_cast_ops() {
  sum_numeric_or_na \
    "$(ir_metric_field "$1" "op.memref.reinterpret_cast" "0")" \
    "$(ir_metric_field "$1" "op.d_memref.reinterpret_cast" "0")"
}

count_source_subview_ops() {
  sum_numeric_or_na \
    "$(ir_metric_field "$1" "op.memref.subview" "0")" \
    "$(ir_metric_field "$1" "op.d_memref.subview" "0")"
}

count_source_extract_strided_metadata_ops() {
  sum_numeric_or_na \
    "$(ir_metric_field "$1" "op.memref.extract_strided_metadata" "0")" \
    "$(ir_metric_field "$1" "op.d_memref.extract_strided_metadata" "0")"
}

count_source_memref_load_ops() {
  ir_metric_field "$1" "op.memref.load" "0"
}

count_source_memref_store_ops() {
  ir_metric_field "$1" "op.memref.store" "0"
}

count_source_d_memref_load_ops() {
  ir_metric_field "$1" "op.d_memref.load" "0"
}

count_source_d_memref_store_ops() {
  ir_metric_field "$1" "op.d_memref.store" "0"
}

count_llvm_calls() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  local count
  count=$(rg -o ' call ' "$path" | wc -l | tr -d ' ')
  echo "${count:-0}"
}

count_shape_index_arith_ops() {
  count_matches 'arith\.(add|mul)i' "$1"
}

count_d_affine_for_ops() {
  count_matches 'd_affine\.for' "$1"
}

count_affine_for_ops() {
  count_matches '(^|[^A-Za-z0-9_])affine\.for' "$1"
}

count_arith_minsi_ops() {
  count_matches 'arith\.min(si|ui)' "$1"
}

count_affine_min_ops() {
  count_matches 'affine\.min| to min ' "$1"
}

count_d_affine_min_ops() {
  count_matches 'd_affine\.min' "$1"
}

count_min_ops() {
  sum_numeric_or_na \
    "$(count_arith_minsi_ops "$1")" \
    "$(count_affine_min_ops "$1")" \
    "$(count_d_affine_min_ops "$1")"
}

count_dynamic_step_ops() {
  count_matches 'step %[A-Za-z0-9_]+' "$1"
}

count_static_step_ops() {
  count_matches 'step [0-9]+' "$1"
}

count_cf_assert_ops() {
  count_matches 'cf\.assert' "$1"
}

count_llvm_cond_br_ops() {
  count_matches 'llvm\.cond_br' "$1"
}

count_abort_calls() {
  count_matches 'llvm\.call @abort|callee = @abort' "$1"
}

metric_field() {
  local key="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  awk -F= -v target="$key" '$1 == target { value=$2 } END { if (value != "") print value }' "$path"
}

sum_numeric_or_na() {
  local total=0
  local saw=0
  local value
  for value in "$@"; do
    if [[ "$value" == "NA" || -z "$value" ]]; then
      continue
    fi
    total=$((total + value))
    saw=1
  done
  if [[ $saw -eq 0 ]]; then
    echo "NA"
  else
    echo "$total"
  fi
}

sum_decimal_or_na() {
  local saw=0
  local value
  local joined=""
  for value in "$@"; do
    if [[ "$value" == "NA" || -z "$value" ]]; then
      continue
    fi
    if [[ -n "$joined" ]]; then
      joined+=" "
    fi
    joined+="$value"
    saw=1
  done
  if [[ $saw -eq 0 ]]; then
    echo "NA"
    return
  fi
  awk -v values="$joined" 'BEGIN {
    n = split(values, parts, /[[:space:]]+/)
    sum = 0.0
    for (i = 1; i <= n; ++i) {
      if (parts[i] != "") sum += parts[i]
    }
    printf "%.2f\n", sum
  }'
}

json_top_level_field() {
  local path="$1"
  local key="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  python3 - "$path" "$key" <<'PY'
import json
import sys

path, key = sys.argv[1], sys.argv[2]
with open(path, "r", encoding="utf-8") as f:
    payload = json.load(f)
value = payload.get(key, "NA")
print(value if value is not None else "NA")
PY
}

json_stage_elapsed_ms() {
  local path="$1"
  local label="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi
  python3 - "$path" "$label" <<'PY'
import json
import sys

path, label = sys.argv[1], sys.argv[2]
with open(path, "r", encoding="utf-8") as f:
    payload = json.load(f)
for stage in payload.get("stages", []):
    if stage.get("label") == label:
        value = stage.get("elapsed_ms", "NA")
        print(value if value is not None else "NA")
        break
else:
    print("NA")
PY
}

write_metrics_csv_header() {
  local path="$1"
  printf '%s\n' "$COMMON_METRICS_HEADER" > "$path"
}

append_metrics_csv_row() {
  local path="$1"
  shift
  local expected
  local actual=$#
  local sanitized=()
  local value
  expected="$(header_column_count)"
  while [[ $actual -lt $expected ]]; do
    set -- "$@" "NA"
    actual=$((actual + 1))
  done
  for value in "$@"; do
    value="${value//$'\n'/ }"
    value="${value//$'\r'/ }"
    value="${value//,/;}"
    sanitized+=("$value")
  done
  printf '%s\n' "$(IFS=,; echo "${sanitized[*]}")" >> "$path"
}

write_summary_header() {
  local path="$1"
  local title="$2"
  cat > "$path" <<EOF
# $title

| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
EOF
}

append_summary_row() {
  local path="$1"
  shift
  printf '| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n' "$@" >> "$path"
}

append_summary_metric_notes() {
  local path="$1"
  cat >> "$path" <<'EOF'

## Metric Definitions

- `Benchmark`: benchmark or benchmark family member represented by the row.
- `Variant`: implementation route being compared, for example `mlir_baseline`, `scair_baseline`, `debruijn`, or `value_dependent`.
- `Rep`: representation-specific note for the row. For selector experiments this records the selector setting, such as `selector=0` or `selector=1`.
- `Build`: build outcome for the benchmark artifact. `ok` means the benchmark built successfully. `unsupported` means the pipeline failed or the route is not currently supported.
- `Run`: benchmark execution outcome. `ok` means the executable ran and produced timing/result data. `NA` means no run data was produced.
- `Structural ops`: total parsed IR operation nodes in the measured source IR. This is a parser-backed structural count, not a line count and not a regex/text estimate.
- `Func defs`: parsed count of function definition operations in the measured IR, currently `func.func` plus `llvm.func`.
- `Block args`: parsed count of SSA block arguments across all blocks in the measured IR.
- `MLIR LOC`: line count of the emitted lowered MLIR artifact on disk, measured with `wc -l`. This is a textual file metric taken after the MLIR file has been generated.
- `LLVM LOC`: line count of the emitted LLVM IR `.ll` artifact on disk, measured with `wc -l`. This is a textual file metric taken after the LLVM IR file has been generated.
- `Compile ms`: wall-clock build time for the benchmark pipeline, reported in milliseconds.
- `Result`: observed benchmark result value produced by the executable.
- `Expected`: expected benchmark result used as a correctness check.
- `ns/iter`: median runtime in nanoseconds per iteration across repeated benchmark runs.
- `parse_time_ms`, `verification_time_ms`, `lowering_time_ms`, `compile_total_ms`: compile-time split for routes that expose stage timing. `NA` means the split is not available yet for that family.
- `runtime_iqr_ns_per_iter`: interquartile range across the recorded repetitions.
- `checksum`: correctness guard value recorded by the benchmark driver when available.
- `checksum_status`: `ok`, `fail`, or `NA` depending on whether a checksum-based validation was emitted.
- `env_path`: captured environment snapshot for the benchmark family output directory.
- `raw_timings_path`: raw per-repetition timing samples in nanoseconds per iteration.
EOF
}

numeric_series_stat() {
  local mode="$1"
  local path="$2"
  if [[ ! -f "$path" ]]; then
    echo "NA"
    return
  fi

  awk -v mode="$mode" '
    BEGIN { count = 0 }
    /^[[:space:]]*$/ { next }
    {
      vals[count] = $1 + 0
      count++
    }
    END {
      if (count == 0) {
        print "NA"
        exit
      }
      for (i = 0; i < count; ++i) {
        for (j = i + 1; j < count; ++j) {
          if (vals[j] < vals[i]) {
            tmp = vals[i]
            vals[i] = vals[j]
            vals[j] = tmp
          }
        }
      }
      if (mode == "min") {
        printf "%.2f\n", vals[0]
      } else if (mode == "max") {
        printf "%.2f\n", vals[count - 1]
      } else if (mode == "q1" || mode == "q3") {
        split_point = int(count / 2)
        if ((count % 2) == 1) {
          low_count = split_point
          high_start = split_point + 1
          high_count = split_point
        } else {
          low_count = split_point
          high_start = split_point
          high_count = split_point
        }

        if (mode == "q1") {
          if (low_count == 0) {
            printf "%.2f\n", vals[0]
          } else if ((low_count % 2) == 1) {
            printf "%.2f\n", vals[int(low_count / 2)]
          } else {
            idx = int(low_count / 2)
            printf "%.2f\n", (vals[idx - 1] + vals[idx]) / 2.0
          }
        } else {
          if (high_count == 0) {
            printf "%.2f\n", vals[count - 1]
          } else if ((high_count % 2) == 1) {
            idx = high_start + int(high_count / 2)
            printf "%.2f\n", vals[idx]
          } else {
            idx = high_start + int(high_count / 2)
            printf "%.2f\n", (vals[idx - 1] + vals[idx]) / 2.0
          }
        }
      } else {
        mid = int(count / 2)
        if ((count % 2) == 1) {
          median = vals[mid]
        } else {
          median = (vals[mid - 1] + vals[mid]) / 2.0
        }
        printf "%.2f\n", median
      }
    }
  ' "$path"
}

benchmark_progress() {
  local label="$1"
  local phase="$2"
  local current="$3"
  local total="$4"
  local status="${5:-}"

  if [[ "${BENCH_PROGRESS:-1}" == "0" || "$total" -le 0 ]]; then
    return
  fi

  local width=28
  local filled=$((current * width / total))
  local empty=$((width - filled))
  local bar_fill
  local bar_empty
  printf -v bar_fill '%*s' "$filled" ''
  printf -v bar_empty '%*s' "$empty" ''
  bar_fill="${bar_fill// /#}"
  bar_empty="${bar_empty// /.}"

  if [[ -t 2 ]]; then
    printf '\r[%s%s] %s %s %d/%d %s' "$bar_fill" "$bar_empty" "$label" "$phase" "$current" "$total" "$status" >&2
    if [[ "$current" -ge "$total" && "$status" != "running" ]]; then
      printf '\n' >&2
    fi
  else
    printf '[%s%s] %s %s %d/%d %s\n' "$bar_fill" "$bar_empty" "$label" "$phase" "$current" "$total" "$status" >&2
  fi
}

run_benchmark_repeated() {
  local output_txt="$1"
  shift

  local warmups="${BENCH_WARMUP_REPS:-1}"
  local reps="${BENCH_TIMING_REPS:-7}"
  local progress_label
  local tmp_dir
  local rep_out
  local ns_values
  local last_out
  local rep
  local ns
  local result
  local expected
  local benchmark
  local variant
  local iterations
  local median_ns
  local min_ns
  local max_ns
  local q1_ns
  local q3_ns
  local iqr_ns
  local checksum
  local checksum_status
  local raw_timings_path

  tmp_dir="$(mktemp -d)"
  ns_values="$tmp_dir/ns_values.txt"
  : > "$ns_values"
  raw_timings_path="${output_txt%.txt}.timings.txt"
  progress_label="$(basename "${output_txt%.txt}")"

  for ((rep = 0; rep < warmups; ++rep)); do
    benchmark_progress "$progress_label" "warmup" "$rep" "$warmups" "running"
    "$@" > /dev/null
    benchmark_progress "$progress_label" "warmup" "$((rep + 1))" "$warmups" "done"
  done

  for ((rep = 1; rep <= reps; ++rep)); do
    rep_out="$tmp_dir/rep_${rep}.txt"
    benchmark_progress "$progress_label" "measure" "$((rep - 1))" "$reps" "running"
    "$@" > "$rep_out"
    ns="$(metric_field ns_per_iter "$rep_out")"
    if [[ -z "$ns" || "$ns" == "NA" ]]; then
      echo "error: benchmark output missing ns_per_iter from: $*" >&2
      rm -rf "$tmp_dir"
      exit 1
    fi
    printf '%s\n' "$ns" >> "$ns_values"
    last_out="$rep_out"
    benchmark_progress "$progress_label" "measure" "$rep" "$reps" "done"
  done

  benchmark="$(metric_field benchmark "$last_out")"
  variant="$(metric_field variant "$last_out")"
  iterations="$(metric_field iterations "$last_out")"
  result="$(metric_field result "$last_out")"
  expected="$(metric_field expected_result "$last_out")"
  checksum="$(metric_field checksum "$last_out")"
  median_ns="$(numeric_series_stat median "$ns_values")"
  min_ns="$(numeric_series_stat min "$ns_values")"
  max_ns="$(numeric_series_stat max "$ns_values")"
  q1_ns="$(numeric_series_stat q1 "$ns_values")"
  q3_ns="$(numeric_series_stat q3 "$ns_values")"
  iqr_ns="$(awk -v q1="$q1_ns" -v q3="$q3_ns" 'BEGIN { if (q1 == "NA" || q3 == "NA") print "NA"; else printf "%.2f\n", q3 - q1 }')"
  cp "$ns_values" "$raw_timings_path"
  require_nonempty_file "$raw_timings_path"

  if [[ -n "$checksum" ]]; then
    checksum_status="ok"
  else
    checksum_status="NA"
  fi

  cat "$last_out" > "$output_txt"
  {
    echo "run_status=ok"
    echo "benchmark_repetitions=$reps"
    echo "benchmark_warmups=$warmups"
    if [[ -n "$benchmark" ]]; then
      echo "benchmark=$benchmark"
    fi
    if [[ -n "$variant" ]]; then
      echo "variant=$variant"
    fi
    if [[ -n "$result" ]]; then
      echo "result=$result"
    fi
    if [[ -n "$expected" ]]; then
      echo "expected_result=$expected"
    fi
    if [[ -n "$checksum" ]]; then
      echo "checksum=$checksum"
      echo "checksum_status=$checksum_status"
    fi
    if [[ -n "$iterations" ]]; then
      echo "iterations=$iterations"
    fi
    echo "timing_min_ns_per_iter=$min_ns"
    echo "timing_max_ns_per_iter=$max_ns"
    echo "timing_q1_ns_per_iter=$q1_ns"
    echo "timing_q3_ns_per_iter=$q3_ns"
    echo "runtime_iqr_ns_per_iter=$iqr_ns"
    echo "runtime_ns_per_iter=$median_ns"
    echo "ns_per_iter=$median_ns"
    echo "raw_timings_path=$raw_timings_path"
  } >> "$output_txt"

  rm -rf "$tmp_dir"
}

capture_env_snapshot() {
  local out_path="$1"
  local script_path="$SCAIR_ROOT/experiments/collect_env.py"
  if [[ -f "$script_path" ]]; then
    python3 "$script_path" "$out_path"
  else
    cat > "$out_path" <<EOF
{"status":"missing_collect_env"}
EOF
  fi
}

ensure_env_snapshot() {
  local out_dir="$1"
  local env_path="$out_dir/env.json"
  capture_env_snapshot "$env_path"
  echo "$env_path"
}

git_commit_for_metrics() {
  git rev-parse HEAD 2>/dev/null || echo "NA"
}

machine_id_for_metrics() {
  local path="/etc/machine-id"
  if [[ -f "$path" ]]; then
    tr -d '\n' < "$path"
  else
    hostname
  fi
}
