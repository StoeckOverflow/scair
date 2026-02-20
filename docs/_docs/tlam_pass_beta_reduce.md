---
title: "TLam Pass: beta-reduce-tlam"
---

# Pass: `beta-reduce-tlam`

Implementation: `dialects/src/tlam/BetaReduceTlamPass.scala`

## What the pass does

It performs local value-level beta reduction:
- pattern: `tlam.vapply` where callee is directly produced by `tlam.vlambda`
- rewrite: inline cloned lambda body before the apply site, map parameter to argument, replace apply result with mapped return value, erase `vapply`

This is value-level inlining, not type-level forall instantiation.

## Pass structure

1. `transform` runs only for `ModuleOp`.
2. `run` walks all regions recursively.
3. It iterates to fixed point (`while changed`) so newly exposed opportunities can reduce in the same pass run.

## Matching and safety guards

`betaReduce(app: VApply)` requires:
1. `app.fun.owner` is `VLambda`.
2. Lambda body has one block, one argument, and ends with `VReturn`.
3. All non-terminator body ops are memory-effect free by `isMemoryEffectFreeOp`.
4. If argument producer is effectful and lambda parameter use count is greater than one, do not reduce.

Effect model in code:
- pure: ops with `NoMemoryEffect`, plus `VLambda`, `TLambda`, `TApply` (explicitly treated as pure here)
- effectful: everything else

## How substitution is implemented

The pass uses IR cloning plus value mapping, not textual replacement.

Steps:
1. Build `valueMapper` and seed with `param -> app.arg`.
2. Clone lambda body prefix ops (`deepCopy`).
3. Insert clones immediately before the original `vapply`.
4. Resolve mapped return value from the mapper.
5. Replace all uses of `app.res` via `RewriteMethods.replaceValue`.
6. Erase original `vapply`.

Because clone/remap works at IR value level, nested regions and SSA identity are handled safely.

## Why type-embedded uses are considered

`countValueUsesInOpTree` counts parameter uses from:
- operands
- result/operand types
- attributes
- properties
- nested regions

It uses `AttributeWalker.foreachValueAttribute`, so `tvar(%x)` occurrences inside types/attrs/properties are included in duplication safety checks.

## Failure/skip behavior

If any precondition fails, the pass simply skips that `vapply` and leaves IR unchanged.
