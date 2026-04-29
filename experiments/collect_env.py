#!/usr/bin/env python3

import json
import os
import platform
import socket
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


def command_output(args):
    try:
        return subprocess.check_output(args, text=True, stderr=subprocess.DEVNULL).strip()
    except Exception:
        return "unavailable"


def read_text_if_exists(path: str):
    try:
        value = Path(path).read_text(encoding="utf-8").strip()
        return value if value else "unknown"
    except Exception:
        return "unavailable"


def first_available(*values: str):
    for value in values:
        if value and value not in {"", "unknown", "unavailable", "NA"}:
            return value
    return "unknown"


def read_machine_id():
    machine_id = Path("/etc/machine-id")
    if machine_id.exists():
        return machine_id.read_text(encoding="utf-8").strip()
    return socket.gethostname()


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: collect_env.py <out.json>", file=sys.stderr)
        return 2

    out_path = Path(sys.argv[1])
    out_path.parent.mkdir(parents=True, exist_ok=True)

    payload = {
        "captured_at_utc": datetime.now(timezone.utc).isoformat(),
        "hostname": socket.gethostname(),
        "machine_id": read_machine_id(),
        "os": platform.platform(),
        "kernel": platform.release(),
        "arch": platform.machine(),
        "python": platform.python_version(),
        "cpu_model": command_output(["bash", "-lc", "grep -m1 'model name' /proc/cpuinfo | cut -d: -f2- | sed 's/^ //'"]),
        "clang_version": command_output(["bash", "-lc", "$HOME/dev/llvm-source/build/bin/clang --version | head -n 1"]),
        "mlir_opt_version": command_output(["bash", "-lc", "$HOME/dev/llvm-source/build/bin/mlir-opt --version | head -n 1"]),
        "git_commit": command_output(["git", "rev-parse", "HEAD"]),
        "omp_num_threads": os.environ.get("OMP_NUM_THREADS", "NA"),
        "mlir_num_threads": os.environ.get("MLIR_NUM_THREADS", "NA"),
        "openblas_num_threads": os.environ.get("OPENBLAS_NUM_THREADS", "NA"),
        "mkl_num_threads": os.environ.get("MKL_NUM_THREADS", "NA"),
        "veclib_maximum_threads": os.environ.get("VECLIB_MAXIMUM_THREADS", "NA"),
        "numexpr_num_threads": os.environ.get("NUMEXPR_NUM_THREADS", "NA"),
        "cc": os.environ.get("CC", "NA"),
        "llvm_build_dir": os.environ.get("LLVM_BUILD_DIR", str(Path.home() / "dev/llvm-source/build")),
        "java_tool_options": os.environ.get("JAVA_TOOL_OPTIONS", "NA"),
        "jdk_java_options": os.environ.get("JDK_JAVA_OPTIONS", "NA"),
        "_java_options": os.environ.get("_JAVA_OPTIONS", "NA"),
        "cpu_governor": read_text_if_exists("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"),
        "cpu_boost": first_available(
            read_text_if_exists("/sys/devices/system/cpu/intel_pstate/no_turbo"),
            read_text_if_exists("/sys/devices/system/cpu/cpufreq/boost"),
        ),
        "cpu_affinity": command_output(["bash", "-lc", "taskset -pc $$ | cut -d: -f2- | sed 's/^ //'"]),
    }
    out_path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
