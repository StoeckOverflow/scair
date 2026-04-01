#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"

MATMUL_SRC="${MATMUL_SRC:-matmul_kernel_scair.mlir}"
CHECKSUM_SRC="${CHECKSUM_SRC:-checksum_kernel_scair.mlir}"
DRIVER_SRC="${DRIVER_SRC:-driver.c}"

OUT_DIR="${OUT_DIR:-build_scair}"

mkdir -p "$OUT_DIR"

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

require_bin "$SCAIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"

require_file "$MATMUL_SRC"
require_file "$CHECKSUM_SRC"
require_file "$DRIVER_SRC"

build_kernel() {
  local route="$1"
  local src="$2"
  local out="$3"

  "$SCAIR_OPT" "$src" --passes "$route,convert-func-to-llvm,convert-llvm-export-abi" \
    | "$MLIR_TRANSLATE" --mlir-to-llvmir \
    | "$CC" -O2 -x ir - -c -o "$out"
}

echo "==> Building ScaIR baseline split kernels"
build_kernel "lower-dynamic-memref-to-llvm-baseline" "$MATMUL_SRC" "$OUT_DIR/matmul_baseline_scair.o"
build_kernel "lower-dynamic-memref-to-llvm-baseline" "$CHECKSUM_SRC" "$OUT_DIR/checksum_baseline_scair.o"

echo "==> Linking ScaIR baseline executable"
"$CC" -O2 "$DRIVER_SRC" \
  "$OUT_DIR/matmul_baseline_scair.o" \
  "$OUT_DIR/checksum_baseline_scair.o" \
  -o "$OUT_DIR/matmul_baseline_scair_exec"

echo "==> Building ScaIR value-dependent split kernels"
build_kernel "lower-dynamic-memref-to-llvm" "$MATMUL_SRC" "$OUT_DIR/matmul_value_dependent_scair.o"
build_kernel "lower-dynamic-memref-to-llvm" "$CHECKSUM_SRC" "$OUT_DIR/checksum_value_dependent_scair.o"

echo "==> Linking ScaIR value-dependent executable"
"$CC" -O2 "$DRIVER_SRC" \
  "$OUT_DIR/matmul_value_dependent_scair.o" \
  "$OUT_DIR/checksum_value_dependent_scair.o" \
  -o "$OUT_DIR/matmul_value_dependent_scair_exec"

echo
echo "ScaIR build complete."
echo "Produced:"
echo "  $OUT_DIR/matmul_baseline_scair_exec"
echo "  $OUT_DIR/matmul_value_dependent_scair_exec"
