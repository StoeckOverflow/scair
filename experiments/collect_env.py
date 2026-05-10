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


def first_cpu_from_list(value: str) -> str:
    if not value or value in {"NA", "unknown", "unavailable"}:
        return "0"
    first = value.split(",", 1)[0].strip()
    if "-" in first:
        first = first.split("-", 1)[0].strip()
    return first if first.isdigit() else "0"


def tool_version(llvm_build_dir: str, tool: str) -> str:
    tool_path = Path(llvm_build_dir) / "bin" / tool
    if not tool_path.exists():
        return "unavailable"
    lines = command_output([str(tool_path), "--version"]).splitlines()
    for line in lines:
        if "version" in line.lower():
            return line
    return lines[0] if lines else "unavailable"


def read_cache_hierarchy(cpu: str):
    cache_root = Path(f"/sys/devices/system/cpu/cpu{cpu}/cache")
    entries = []
    if not cache_root.exists():
        return {"cpu": cpu, "status": "unavailable", "entries": entries}

    for index in sorted(cache_root.glob("index*")):
        entries.append(
            {
                "index": index.name,
                "level": read_text_if_exists(str(index / "level")),
                "type": read_text_if_exists(str(index / "type")),
                "size": read_text_if_exists(str(index / "size")),
                "shared_cpu_list": read_text_if_exists(str(index / "shared_cpu_list")),
            }
        )
    return {"cpu": cpu, "status": "ok", "entries": entries}


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

    llvm_build_dir = os.environ.get("LLVM_BUILD_DIR", str(Path.home() / "dev/llvm-clean-build"))
    cpu_affinity = command_output(["bash", "-lc", "taskset -pc $$ | cut -d: -f2- | sed 's/^ //'"])
    bench_cpu_pin = os.environ.get("BENCH_CPU_PIN", "NA")
    cache_cpu = first_cpu_from_list(bench_cpu_pin if bench_cpu_pin != "NA" else cpu_affinity)

    payload = {
        "captured_at_utc": datetime.now(timezone.utc).isoformat(),
        "hostname": socket.gethostname(),
        "machine_id": read_machine_id(),
        "os": platform.platform(),
        "kernel": platform.release(),
        "arch": platform.machine(),
        "python": platform.python_version(),
        "cpu_model": command_output(["bash", "-lc", "grep -m1 'model name' /proc/cpuinfo | cut -d: -f2- | sed 's/^ //'"]),
        "clang_version": tool_version(llvm_build_dir, "clang"),
        "mlir_opt_version": tool_version(llvm_build_dir, "mlir-opt"),
        "mlir_translate_version": tool_version(llvm_build_dir, "mlir-translate"),
        "git_commit": command_output(["git", "rev-parse", "HEAD"]),
        "omp_num_threads": os.environ.get("OMP_NUM_THREADS", "NA"),
        "mlir_num_threads": os.environ.get("MLIR_NUM_THREADS", "NA"),
        "openblas_num_threads": os.environ.get("OPENBLAS_NUM_THREADS", "NA"),
        "mkl_num_threads": os.environ.get("MKL_NUM_THREADS", "NA"),
        "veclib_maximum_threads": os.environ.get("VECLIB_MAXIMUM_THREADS", "NA"),
        "numexpr_num_threads": os.environ.get("NUMEXPR_NUM_THREADS", "NA"),
        "cc": os.environ.get("CC", "NA"),
        "llvm_build_dir": llvm_build_dir,
        "java_tool_options": os.environ.get("JAVA_TOOL_OPTIONS", "NA"),
        "jdk_java_options": os.environ.get("JDK_JAVA_OPTIONS", "NA"),
        "_java_options": os.environ.get("_JAVA_OPTIONS", "NA"),
        "cpu_governor": read_text_if_exists("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"),
        "cpu_boost": first_available(
            read_text_if_exists("/sys/devices/system/cpu/intel_pstate/no_turbo"),
            read_text_if_exists("/sys/devices/system/cpu/cpufreq/boost"),
        ),
        "bench_cpu_pin": bench_cpu_pin,
        "cpu_affinity": cpu_affinity,
        "cache_hierarchy": read_cache_hierarchy(cache_cpu),
    }
    out_path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
