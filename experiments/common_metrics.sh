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
COMMON_METRICS_HEADER="experiment_family,benchmark,variant,representation_group,build_status,run_status,source_bytes,source_loc,source_ops,source_ops_structural,source_func_defs,source_block_args,source_alloc_ops,source_reinterpret_cast_ops,source_subview_ops,source_extract_strided_metadata_ops,source_memref_load_ops,source_memref_store_ops,source_dmemref_load_ops,source_dmemref_store_ops,lowered_func_defs,lowered_ops,lowered_ops_structural,lowered_mlir_lines,llvm_ir_lines,llvm_call_count,compile_ms,result,expected_result,runtime_ns_per_iter,notes,source_helper_defs,bvar_refs,value_ssa_refs,opt_llvm_lines,opt_llvm_call_count"

BENCH_WARMUP_REPS="${BENCH_WARMUP_REPS:-1}"
BENCH_TIMING_REPS="${BENCH_TIMING_REPS:-7}"

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

  local llvm_build="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
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

count_source_dmemref_load_ops() {
  ir_metric_field "$1" "op.d_memref.load" "0"
}

count_source_dmemref_store_ops() {
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

write_metrics_csv_header() {
  local path="$1"
  printf '%s\n' "$COMMON_METRICS_HEADER" > "$path"
}

append_metrics_csv_row() {
  local path="$1"
  shift
  printf '%s\n' "$(IFS=,; echo "$*")" >> "$path"
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

run_benchmark_repeated() {
  local output_txt="$1"
  shift

  local warmups="${BENCH_WARMUP_REPS:-1}"
  local reps="${BENCH_TIMING_REPS:-7}"
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

  tmp_dir="$(mktemp -d)"
  ns_values="$tmp_dir/ns_values.txt"
  : > "$ns_values"

  for ((rep = 0; rep < warmups; ++rep)); do
    "$@" > /dev/null
  done

  for ((rep = 1; rep <= reps; ++rep)); do
    rep_out="$tmp_dir/rep_${rep}.txt"
    "$@" > "$rep_out"
    ns="$(metric_field ns_per_iter "$rep_out")"
    if [[ -z "$ns" || "$ns" == "NA" ]]; then
      echo "error: benchmark output missing ns_per_iter from: $*" >&2
      rm -rf "$tmp_dir"
      exit 1
    fi
    printf '%s\n' "$ns" >> "$ns_values"
    last_out="$rep_out"
  done

  benchmark="$(metric_field benchmark "$last_out")"
  variant="$(metric_field variant "$last_out")"
  iterations="$(metric_field iterations "$last_out")"
  result="$(metric_field result "$last_out")"
  expected="$(metric_field expected_result "$last_out")"
  median_ns="$(numeric_series_stat median "$ns_values")"
  min_ns="$(numeric_series_stat min "$ns_values")"
  max_ns="$(numeric_series_stat max "$ns_values")"

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
    if [[ -n "$iterations" ]]; then
      echo "iterations=$iterations"
    fi
    echo "timing_min_ns_per_iter=$min_ns"
    echo "timing_max_ns_per_iter=$max_ns"
    echo "runtime_ns_per_iter=$median_ns"
    echo "ns_per_iter=$median_ns"
  } >> "$output_txt"

  rm -rf "$tmp_dir"
}
