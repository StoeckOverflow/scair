#!/usr/bin/env bash
set -euo pipefail

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"

SCAIR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCAIR_OPT="${SCAIR_ROOT}/out/tools/opt/launcher.dest/run"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"

BASELINE_SRC="${BASELINE_SRC:-$EXAMPLE_DIR/semi_affine_kernel_scair_baseline_bare.mlir}"
VALUE_DEP_SRC="${VALUE_DEP_SRC:-$EXAMPLE_DIR/semi_affine_kernel_scair_bare.mlir}"
BASELINE_DRIVER_SRC="${BASELINE_DRIVER_SRC:-$EXAMPLE_DIR/driver_baseline_bare.c}"
VALUE_DEP_DRIVER_SRC="${VALUE_DEP_DRIVER_SRC:-$EXAMPLE_DIR/driver_bare.c}"

OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/build_scair}"

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

require_file "$BASELINE_SRC"
require_file "$VALUE_DEP_SRC"
require_file "$BASELINE_DRIVER_SRC"
require_file "$VALUE_DEP_DRIVER_SRC"

build_kernel() {
  local route="$1"
  local src="$2"
  local out="$3"
  local llvm_ir_out="$4"

  "$SCAIR_OPT" "$src" --passes "$route,convert-func-to-llvm,convert-llvm-export-abi" \
    | "$MLIR_TRANSLATE" --mlir-to-llvmir \
    > "$llvm_ir_out"
  "$CC" -O2 -x ir "$llvm_ir_out" -c -o "$out"
}

echo "==> Building ScaIR semi-affine baseline kernel-only"
build_kernel \
  "lower-dynamic-memref-to-llvm-baseline" \
  "$BASELINE_SRC" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.o" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.ll"

echo "==> Linking ScaIR semi-affine baseline kernel-only executable"
"$CC" -O2 "$BASELINE_DRIVER_SRC" \
  "$OUT_DIR/semi_affine_baseline_kernel_only_scair.o" \
  -o "$OUT_DIR/semi_affine_baseline_kernel_only_scair_exec"

echo "==> Building ScaIR semi-affine value-dependent kernel-only"
build_kernel \
  "lower-dynamic-memref-to-llvm" \
  "$VALUE_DEP_SRC" \
  "$OUT_DIR/semi_affine_value_dependent_scair.o" \
  "$OUT_DIR/semi_affine_value_dependent_scair.ll"

echo "==> Linking ScaIR semi-affine value-dependent executable"
"$CC" -O2 "$VALUE_DEP_DRIVER_SRC" \
  "$OUT_DIR/semi_affine_value_dependent_scair.o" \
  -o "$OUT_DIR/semi_affine_value_dependent_scair_exec"

echo
echo "ScaIR semi-affine build complete."
echo "Produced:"
echo "  $OUT_DIR/semi_affine_baseline_kernel_only_scair_exec"
echo "  $OUT_DIR/semi_affine_value_dependent_scair_exec"
echo "  $OUT_DIR/semi_affine_baseline_kernel_only_scair.ll"
echo "  $OUT_DIR/semi_affine_value_dependent_scair.ll"
