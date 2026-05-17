#!/usr/bin/env python3

import csv
import sys
from collections import defaultdict
from pathlib import Path


FAMILY_ORDER = {
    "strided_matmul_benchmark": 0,
    "convolution_benchmark": 1,
    "matmul_reduction_dim_tiling_benchmark": 2,
    "broadcast_affine_2d_benchmark": 3,
    "semi_affine_indexing_benchmark": 4,
    "type_polymorphism": 5,
}


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: summarize_results.py <all_metrics.csv> <summary.md>", file=sys.stderr)
        return 2

    csv_path = Path(sys.argv[1])
    md_path = Path(sys.argv[2])
    rows = list(csv.DictReader(csv_path.open(newline="", encoding="utf-8")))
    groups = defaultdict(list)
    for row in rows:
        groups[row["experiment_family"]].append(row)

    variant_order = {
        "mlir_baseline": 0,
        "scair_baseline": 1,
        "debruijn": 1,
        "scair_dmemref": 2,
        "value_dependent": 2,
    }

    with md_path.open("w", encoding="utf-8") as out:
        out.write("# Uniform Experiment Metrics Summary\n\n")
        for family in sorted(groups, key=lambda name: (FAMILY_ORDER.get(name, 99), name)):
            out.write(f"## {family}\n\n")
            out.write("| Benchmark | Variant | Kernel | Size | Reps | Build | Run | Verify ms | Lowering pipeline ms | Staged total ms | Timed median ns/iter | Timed IQR ns/iter | Checksum | Checksum status | Commit | Env |\n")
            out.write("| --- | --- | --- | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | --- | --- |\n")
            for row in sorted(groups[family], key=lambda row: (row["benchmark"], variant_order.get(row["variant"], 99), row["representation_group"])):
                out.write(
                    f"| {row['benchmark']} | {row['variant']} | {row.get('kernel', 'NA')} | {row.get('size', 'NA')} | "
                    f"{row.get('benchmark_repetitions', 'NA')} | {row['build_status']} | {row['run_status']} | {row.get('verification_time_ms', 'NA')} | "
                    f"{row.get('lowering_time_ms', 'NA')} | {row.get('compile_total_ms', row.get('compile_ms', 'NA'))} | "
                    f"{row.get('runtime_median_ns_per_iter', row.get('runtime_ns_per_iter', 'NA'))} | {row.get('runtime_iqr_ns_per_iter', 'NA')} | "
                    f"{row.get('checksum', 'NA')} | {row.get('checksum_status', 'NA')} | "
                    f"{row.get('git_commit', 'NA')} | {row.get('env_path', 'NA')} |\n"
                )
            out.write("\n")
        out.write("## Notes\n\n")
        out.write("- `Timed median ns/iter` and `Timed IQR ns/iter` describe the benchmark's timed region, which may include required output reset/zeroing in addition to kernel execution.\n")
        out.write("- `verification_time_ms`, `lowering_time_ms`, and `compile_total_ms` are the thesis-facing staged tool timings for upgraded families.\n")
        out.write("- `lowering_time_ms` is an inclusive pipeline run over the source IR, not an isolated pass-only timer.\n")
        out.write("- `compile_total_ms` is the total staged tool time captured by `run_pipeline.py`, not full native-code build time.\n")
        out.write("- `compile_total_ms` falls back to legacy `compile_ms` when a family has not yet been upgraded to split compile timing.\n")
        out.write("- `runtime_median_ns_per_iter` falls back to legacy `runtime_ns_per_iter` for older rows.\n")
        out.write("- Weak structural-only families may still emit `NA` for checksum and compile sub-stages.\n")
        out.write("- `Commit` and `Env` identify the source revision and captured machine metadata for each row.\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
