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

## Important guard: only dead `tlambda` is erased

Current implementation erases a `TLambda` **only if** both are empty:
- `tl.res.uses`
- `tl.res.typeUses`

If a `TLambda` is still referenced (for example by `tlam.tapply`), erase pass leaves it unchanged.

This makes the pass safe to run on partially processed IR.

## Rewrite algorithm

`eraseInModule` recursively walks regions.

On each `TLambda tl`:
1. check liveness guard (`uses` + `typeUses` empty)
2. read first body block if present
3. if last body op is `TReturn`:
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
- Malformed `tlambda` body (missing trailing `treturn`): unchanged; verifier reports the error.

See: `tests/filecheck/dialects/tlam/07_lowering/tlam_erase_safety.mlir`.

## Pipeline role

Typical ordering:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`

`erase-tlam` is the structural cleanup stage between specialization and backend lowering.
