---
title: "TLam Pass: erase-tlam"
---

# Pass: `erase-tlam`

Implementation: `dialects/src/tlam/EraseTlamPass.scala`

## What this pass does

`erase-tlam` removes dead type-level lambda wrappers by splicing their body into the parent block.

For an erasable `TLambda`:
1. move body ops except trailing `tlam.treturn` before the `tlambda`
2. forward `tlambda` result to `treturn` value
3. erase `tlambda`

## Important guards: only dead and non-leaking `tlambda` is erased

Current implementation erases a `TLambda` **only if** both are empty:
- `tl.res.uses`
- `tl.res.typeUses`

If a `TLambda` is still referenced (for example by `tlam.tapply`), erase pass leaves it unchanged.

This makes the pass safe to run on partially processed IR.

There is a second guard as well:
- even for dead `TLambda`, the pass refuses erasure if the binder block argument would leak into moved ops

That leak check scans:
- operands that directly use the binder
- operand types
- result types
- attributes
- properties
- nested regions of moved ops

This specifically protects SSA-in-types payloads such as `!value<%T>` from being moved out of the binder scope.

## Rewrite algorithm

`eraseInModule` recursively walks regions.

On each `TLambda tl`:
1. check liveness guard (`uses` + `typeUses` empty)
2. read first body block if present
3. if last body op is `TReturn`:
   - compute binder-leak guard over body prefix ops and returned value type
   - only if no leak is found:
     - detach body prefix ops (`dropRight(1)`)
     - insert them before `tl`
     - `RewriteMethods.replaceOp(tl, newOps = Seq.empty, newResults = Some(Seq(tret.value)))`
4. otherwise do nothing

Malformed `tlambda` is intentionally left unchanged so verifier diagnostics remain the user-facing signal.

## What this pass does not do

- It does not monomorphize type application (`tlam.tapply`).
- It does not lower value-level TLam ops (`VLambda`, `VApply`, `VReturn`).

Those are handled by `monomorphize` and `lower-tlam-to-func` respectively.

## Behavior on non-ideal input

- Live `tlambda` + `tapply` input: no destructive rewrite.
- Dead `tlambda` whose body still embeds the binder in SSA-in-types payloads: no destructive rewrite.
- Malformed `tlambda` body (missing trailing `treturn`): unchanged; verifier reports the error.

See: `tests/filecheck/dialects/tlam/07_lowering/tlam_erase_safety.mlir`.

## Why the binder-leak guard matters

Without this guard, a dead TLambda like:
```mlir
%dead = "tlam.tlambda"() ({
^bb0(%T: !tlam.type):
  %tv = "builtin.unrealized_conversion_cast"(%T)
      {dep = !tlam.forall<!value<%T>>}
      : (!tlam.type) -> !value<%T>
  %v = "test.make_i64"() : () -> i64
  "tlam.treturn"(%v) : (i64) -> ()
}) : () -> !tlam.forall<i64>
```

would move `%tv` out of the binder scope, leaving `%T` dangling inside:
- the operand list
- the result type
- the attribute payload

Current code keeps that TLambda intact instead.

## Pipeline role

Typical ordering:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`

`erase-tlam` is the structural cleanup stage between specialization and backend lowering.
