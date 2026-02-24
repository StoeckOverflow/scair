---
title: "TLam Pass: beta-reduce-tlam"
---

# Pass: `beta-reduce-tlam`

Implementation: `dialects/src/tlam/BetaReduceTlamPass.scala`

## What this pass does

`beta-reduce-tlam` performs **local value-level** beta-reduction.

Rewrite shape:
1. match `%r = tlam.vapply(%f, %arg)`
2. require `%f` is directly produced by `tlam.vlambda`
3. clone the lambda body (except final `tlam.vreturn`) before `%r`
4. substitute lambda parameter with `%arg` via value mapping
5. replace `%r` with the mapped return value
6. erase the original `tlam.vapply`

This is value-level inlining. It does not perform type-level `forall` instantiation (`tlam.tapply`); that is handled by `monomorphize`.

## Pass structure

`run` walks the module recursively and applies rewrites to fixed point (`while changed`), so reductions exposed by earlier reductions in the same run can also fire.

## Preconditions for one reduction

`betaReduce(app: VApply)` only rewrites when all checks pass:
1. callee producer is exactly a `VLambda`
2. lambda body is a single block with exactly one argument
3. lambda body ends with `VReturn`
4. all non-terminator body ops are memory-effect free by `isMemoryEffectFreeOp`
5. duplication safety: if `%arg` comes from an effectful producer and parameter-use count is greater than 1, skip

If any check fails, the `vapply` is left unchanged.

## Effect model used by the pass

`isMemoryEffectFreeOp` treats as pure:
- ops implementing `NoMemoryEffect`
- `VLambda`, `TLambda`, `TApply` (explicitly treated as effect free here)

Everything else is treated conservatively as effectful.

Notably, nested `vapply` in the lambda body is considered effectful by this model, so such outer calls are not beta-reduced.

## How substitution is implemented

The pass uses cloning + SSA remapping (not textual replacement):
1. initialize `valueMapper` with `param -> app.arg`
2. `deepCopy` body prefix ops
3. insert cloned ops before `app`
4. resolve mapped `vreturn` value through mapper
5. `RewriteMethods.replaceValue(app.res, mappedRet)`
6. erase `app`

This preserves SSA correctness and handles nested regions/types consistently.

## Uses counted for duplication safety

Parameter-use counting (`countValueUsesInOpTree`) includes occurrences in:
- operands
- operand/result types
- attributes
- properties
- nested regions

It uses `AttributeWalker.foreachValueAttribute`, so embedded value references such as `!tlam.tvar<%x>` count too.

## Typical outcomes (from tests)

Will reduce:
- direct identity/application cases
- bodies with only effect-free intermediates
- SSA-in-types cases where values appear in types/attributes

Will not reduce:
- callee not directly from `vlambda`
- body containing effectful/unknown ops
- body containing nested `vapply` (conservative effect model)
- argument from effectful producer when parameter would be duplicated

See: `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_ssa.mlir` and `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_db.mlir`.

## Pipeline role

Often scheduled before or around canonicalization/CSE, then followed by:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`

Both early-beta and late-beta placements are covered by pipeline tests.
