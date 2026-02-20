---
title: "TLam Pass: lower-tlam-to-func"
---

# Pass: `lower-tlam-to-func`

Implementation: `dialects/src/tlam/LowerTlamToFuncPass.scala`

## What the pass does

It lowers value-level TLam constructs into `func` dialect operations.

Main rewrites:
1. `VLambda` is lifted to top-level `func.func @lifted_N` plus `func.constant @lifted_N`
2. `VApply` is rewritten to `func.call_indirect`
3. `VReturn` is rewritten to `func.return`

## Two-phase implementation

## Phase 1: Lambda lifting

While walking regions, for each `VLambda`:
1. generate unique symbol name (`lifted_<counter>`)
2. convert `TlamFunType` to builtin `FunctionType` (`lowerFunType`)
3. detach lambda body region and create `func.func`
4. insert function at start of top module block
5. create `func.constant` referencing the symbol
6. insert constant at start of top module block
7. replace all uses of original lambda value with constant value (`replaceAllUses`)
8. erase original `VLambda`

Why insert at module start: preserve hierarchical dominance for nested uses.

## Phase 2: Rewrite remaining value ops

A greedy pattern rewriter applies:
- `VApply` -> `CallIndirect`
  - requires runtime callee type is builtin `FunctionType`
  - call results come from that function type outputs
- `VReturn` -> `Return`

## Use replacement helper

`replaceAllUses(oldV, newV)` updates:
1. `oldV.typeUses` by rewriting value-attributes and re-registering type uses
2. `oldV.uses` by rebuilding user ops with updated operands

This prevents stale embedded references in type/attribute payloads.

## Error behavior

If a `VApply` callee is not `FunctionType` at lowering time, pass throws with an explicit error message. This indicates pass-ordering or earlier rewrite issues.

## Expected preconditions

- TLam type-level ops should already be erased/specialized for normal pipelines.
- Value-level lambdas/applies remain for this pass to lower.
