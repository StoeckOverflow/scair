#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# Build script for separately compiled MLIR kernels:
#   - matmul_kernel.mlir
#   - checksum_kernel.mlir
# plus a shared C driver:
#   - driver.c
#
# Produces:
#   - matmul_baseline_exec
#   - matmul_tiled_exec
#
# Usage:
#   ./build.sh
#
# Optional environment overrides:
#   LLVM_BUILD_DIR=~/dev/llvm-source/build ./build.sh
#   CC=clang ./build.sh
# -------------------------------------------------------------------

LLVM_BUILD_DIR="${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
BIN_DIR="$LLVM_BUILD_DIR/bin"
CC="${CC:-$BIN_DIR/clang}"

MLIR_OPT="$BIN_DIR/mlir-opt"
MLIR_TRANSLATE="$BIN_DIR/mlir-translate"

MATMUL_SRC="${MATMUL_SRC:-matmul_kernel.mlir}"
CHECKSUM_SRC="${CHECKSUM_SRC:-checksum_kernel.mlir}"
DRIVER_SRC="${DRIVER_SRC:-driver.c}"

OUT_DIR="${OUT_DIR:-build}"

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

require_bin "$MLIR_OPT"
require_bin "$MLIR_TRANSLATE"
require_bin "$CC"

require_file "$MATMUL_SRC"
require_file "$CHECKSUM_SRC"
require_file "$DRIVER_SRC"

echo "==> Using LLVM tools from: $BIN_DIR"
echo "==> Output directory: $OUT_DIR"

# ------------------------------------------------------------
# 1. Build untiled matmul object
# ------------------------------------------------------------
echo "==> Building untiled matmul object"
"$MLIR_OPT" "$MATMUL_SRC" \
  --lower-affine \
  --convert-scf-to-cf \
  --expand-strided-metadata \
  --finalize-memref-to-llvm \
  --convert-arith-to-llvm \
  --convert-index-to-llvm \
  --convert-cf-to-llvm \
  --convert-func-to-llvm \
  --reconcile-unrealized-casts \
| "$MLIR_TRANSLATE" --mlir-to-llvmir \
| "$CC" -O2 -x ir - -c -o "$OUT_DIR/matmul_baseline.o"

# ------------------------------------------------------------
# 2. Tile matmul and build tiled object
# ------------------------------------------------------------
echo "==> Generating tiled matmul IR"
"$MLIR_OPT" "$MATMUL_SRC" \
  -canonicalize \
  -affine-loop-normalize \
  -affine-loop-tile='tile-sizes=32,32,32 separate=true' \
  -affine-loop-fusion \
  -affine-scalrep \
  -canonicalize \
  -o "$OUT_DIR/matmul_tiled.mlir"

echo "==> Building tiled matmul object"
"$MLIR_OPT" "$OUT_DIR/matmul_tiled.mlir" \
  --lower-affine \
  --convert-scf-to-cf \
  --expand-strided-metadata \
  --finalize-memref-to-llvm \
  --convert-arith-to-llvm \
  --convert-index-to-llvm \
  --convert-cf-to-llvm \
  --convert-func-to-llvm \
  --reconcile-unrealized-casts \
| "$MLIR_TRANSLATE" --mlir-to-llvmir \
| "$CC" -O2 -x ir - -c -o "$OUT_DIR/matmul_tiled.o"

# ------------------------------------------------------------
# 3. Build checksum object
# ------------------------------------------------------------
echo "==> Building checksum object"
"$MLIR_OPT" "$CHECKSUM_SRC" \
  --lower-affine \
  --convert-scf-to-cf \
  --expand-strided-metadata \
  --finalize-memref-to-llvm \
  --convert-arith-to-llvm \
  --convert-index-to-llvm \
  --convert-cf-to-llvm \
  --convert-func-to-llvm \
  --reconcile-unrealized-casts \
| "$MLIR_TRANSLATE" --mlir-to-llvmir \
| "$CC" -O2 -x ir - -c -o "$OUT_DIR/checksum.o"

# ------------------------------------------------------------
# 4. Link final executables
# ------------------------------------------------------------
echo "==> Linking untiled executable"
"$CC" -O2 "$DRIVER_SRC" \
  "$OUT_DIR/matmul_baseline.o" \
  "$OUT_DIR/checksum.o" \
  -o "$OUT_DIR/matmul_baseline_exec"

echo "==> Linking tiled executable"
"$CC" -O2 "$DRIVER_SRC" \
  "$OUT_DIR/matmul_tiled.o" \
  "$OUT_DIR/checksum.o" \
  -o "$OUT_DIR/matmul_tiled_exec"

echo
echo "Build complete."
echo "Produced:"
echo "  $OUT_DIR/matmul_baseline_exec"
echo "  $OUT_DIR/matmul_tiled_exec"
echo
echo "Example runs:"
echo "  $OUT_DIR/matmul_baseline_exec 32 32 32"
echo "  $OUT_DIR/matmul_tiled_exec 32 32 32"
echo "  $OUT_DIR/matmul_baseline_exec 64 64 64"
echo "  $OUT_DIR/matmul_tiled_exec 64 64 64"