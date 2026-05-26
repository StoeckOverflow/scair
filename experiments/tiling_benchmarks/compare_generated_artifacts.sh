#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -ne 2 ]]; then
  echo "usage: $0 OLD_DIR NEW_DIR" >&2
  exit 2
fi

old_dir="$1"
new_dir="$2"
status=0

for ext in tiled.mlir llvm.mlir ll; do
  while IFS= read -r old_file; do
    rel="${old_file#$old_dir/}"
    new_file="$new_dir/$rel"
    if [[ ! -f "$new_file" ]]; then
      echo "MISSING $rel"
      status=1
    elif cmp -s "$old_file" "$new_file"; then
      echo "SAME $rel"
    else
      echo "DIFF $rel"
      status=1
    fi
  done < <(find "$old_dir" -type f -name "*.$ext" | sort)
done

exit "$status"
