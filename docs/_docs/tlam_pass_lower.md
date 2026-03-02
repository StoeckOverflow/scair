---
title: "TLam Pass: lower-tlam-to-func"
---

# Pass: `lower-tlam-to-func`

Implementation: `dialects/src/tlam/LowerTlamToFuncPass.scala`

## What this pass does

`lower-tlam-to-func` converts value-level TLam constructs to `func` dialect IR.

Main rewrites:
1. `tlam.vlambda` -> lifted `func.func @lifted_N` + `func.constant @lifted_N`
2. `tlam.vapply` -> `func.call_indirect`
3. `tlam.vreturn` -> `func.return`

## Two-phase lowering

### Phase 1: lambda lifting

For each `VLambda` discovered recursively:
1. build unique symbol name `lifted_<counter>`
2. convert `TlamFunType(in, out)` to builtin `FunctionType(inputs = [in], outputs = [out])`
3. detach/move lambda body region into a new top-level `func.func`
4. create `func.constant` that materializes a first-class function value to that symbol
5. replace all uses of original lambda result with constant result
6. erase original `vlambda`

Both `func.func` and `func.constant` are inserted at the start of the top module block.
This placement ensures hierarchical dominance for uses that may be inside nested regions.

### Phase 2: rewrite remaining value ops

A greedy pattern rewrite converts:
- `VApply` -> `CallIndirect`
- `VReturn` -> `Return`

For `VApply`, runtime callee type is inspected after phase 1 replacement and must be builtin `FunctionType`.
Call result types are taken from that function type outputs.

## Use replacement helper behavior

Phase 1 now uses `RewriteMethods.replaceValue(oldV, newV)` to update both:
1. `oldV.typeUses` (embedded value references in attributes/types)
2. normal operand `uses`

This prevents stale SSA-in-types references during lambda value replacement.

## Error behavior

If a `VApply` callee is not builtin `FunctionType` at rewrite time, pass throws:
`lower-tlam-to-func: expected callee of call_indirect to have builtin.function_type, got ...`

That usually indicates an ordering or invariant violation in earlier stages.

## Preconditions and pipeline position

Typical usage:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`
4. `reconcile-unrealized-casts`

`lower-tlam-to-func` expects TLam type-level control flow to be gone (or irrelevant) and focuses on value-level lowering.

One important value-level invariant is closure-freedom:
- phase 1 physically moves each `vlambda` body into a top-level `func.func`
- `func.func` is verified with `IsolatedFromAbove`
- so any remaining capture of outer SSA values is rejected by the verifier instead of silently producing an invalid lifted function

In other words, this pass implements lambda lifting, not closure conversion.

## Practical outcomes (from tests)

Expected after erase+lower(+reconcile):
- no `tlam.` operations
- no `!tlam.` types
- `func.func`, `func.constant`, `func.call_indirect`, and `func.return` remain

Nested placements (for example under `scf.execute_region`) are also lowered.

`func.call_indirect` is only formed when the callee has builtin `FunctionType`, and the `func` dialect verifier rechecks:
- argument types match the callee input types
- result types match the callee output types

See: `tests/filecheck/dialects/tlam/07_lowering/tlam_no_leftovers_after_erase_lower_reconcile.mlir` and `tests/filecheck/dialects/tlam/99_pipeline/tlam_pipeline_smoke.mlir`.
