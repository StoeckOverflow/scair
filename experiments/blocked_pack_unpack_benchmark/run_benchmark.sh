#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

: "${BLOCKED_PACK_ITERATIONS:=1000}"
: "${BENCH_WARMUP_REPS:=5}"
: "${BENCH_TIMING_REPS:=15}"

export BLOCKED_PACK_ITERATIONS
export BENCH_WARMUP_REPS
export BENCH_TIMING_REPS

echo "Running blocked_pack benchmark with:"
echo "  LLVM_BUILD_DIR=${LLVM_BUILD_DIR:-$HOME/dev/llvm-source/build}"
echo "  BLOCKED_PACK_SIZE_SET=${BLOCKED_PACK_SIZE_SET:-64x64x16x16,128x32x8x32,128x64x16x16}"
echo "  BLOCKED_PACK_ITERATIONS=$BLOCKED_PACK_ITERATIONS"
echo "  BLOCKED_PACK_ROUTES=${BLOCKED_PACK_ROUTES:-mlir_baseline,scair_baseline,value_dependent}"
echo "  BENCH_WARMUP_REPS=$BENCH_WARMUP_REPS"
echo "  BENCH_TIMING_REPS=$BENCH_TIMING_REPS"
echo

bash "$SCRIPT_DIR/build_scair_example.sh"
