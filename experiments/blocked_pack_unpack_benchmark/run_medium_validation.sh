#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export BLOCKED_PACK_SIZE_SET="${BLOCKED_PACK_SIZE_SET:-2x3x4x5,64x64x16x16,128x32x8x32,128x64x16x16}"
export BLOCKED_PACK_ITERATIONS="${BLOCKED_PACK_ITERATIONS:-100}"
export BENCH_WARMUP_REPS="${BENCH_WARMUP_REPS:-5}"
export BENCH_TIMING_REPS="${BENCH_TIMING_REPS:-20}"

echo "Running blocked_pack medium validation with:"
echo "  LLVM_BUILD_DIR=${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
echo "  BLOCKED_PACK_SIZE_SET=$BLOCKED_PACK_SIZE_SET"
echo "  BLOCKED_PACK_ITERATIONS=$BLOCKED_PACK_ITERATIONS"
echo "  BENCH_WARMUP_REPS=$BENCH_WARMUP_REPS"
echo "  BENCH_TIMING_REPS=$BENCH_TIMING_REPS"
echo

bash "$SCRIPT_DIR/build_scair_example.sh"
