#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "usage: $0 <tool> <input.mlir> <out.json> [pipeline]" >&2
  exit 2
fi

TOOL="$1"
INPUT="$2"
OUT_JSON="$3"
PIPELINE="${4:-}"
TOOL_BASENAME="$(basename "$TOOL")"

if [[ -n "$PIPELINE" ]]; then
  if [[ "$TOOL_BASENAME" == "mlir-opt" ]]; then
    VERIFY_CMD="$TOOL \"$INPUT\" > /dev/null"
    LOWER_CMD="$TOOL \"$INPUT\" $PIPELINE > /dev/null"
  else
    VERIFY_CMD="$TOOL -s \"$INPUT\" > /dev/null"
    LOWER_CMD="$TOOL -s \"$INPUT\" --passes \"$PIPELINE\" > /dev/null"
  fi
else
  VERIFY_CMD="$TOOL \"$INPUT\" > /dev/null"
  LOWER_CMD="$TOOL \"$INPUT\" > /dev/null"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
python3 "$SCRIPT_DIR/run_pipeline.py" "$OUT_JSON" \
  verification "$VERIFY_CMD" \
  lowering "$LOWER_CMD"
