#!/usr/bin/env python3

import json
import subprocess
import sys
import time
from pathlib import Path


def run_stage(label: str, command: str):
    start = time.perf_counter_ns()
    completed = subprocess.run(command, shell=True, text=True, capture_output=True)
    end = time.perf_counter_ns()
    return {
        "label": label,
        "command": command,
        "returncode": completed.returncode,
        "elapsed_ms": round((end - start) / 1_000_000.0, 2),
        "stdout": completed.stdout,
        "stderr": completed.stderr,
    }


def main() -> int:
    if len(sys.argv) < 3 or len(sys.argv[2:]) % 2 != 0:
        print("usage: run_pipeline.py <out.json> <label> <command> [<label> <command> ...]", file=sys.stderr)
        return 2

    out_path = Path(sys.argv[1])
    stages = []
    total_start = time.perf_counter_ns()
    for idx in range(2, len(sys.argv), 2):
        stages.append(run_stage(sys.argv[idx], sys.argv[idx + 1]))
        if stages[-1]["returncode"] != 0:
            break
    total_end = time.perf_counter_ns()

    payload = {
        "total_ms": round((total_end - total_start) / 1_000_000.0, 2),
        "stages": stages,
    }
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    return 0 if all(stage["returncode"] == 0 for stage in stages) else 1


if __name__ == "__main__":
    raise SystemExit(main())
