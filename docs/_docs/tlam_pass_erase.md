---
title: "TLam Pass: erase-tlam"
---

# Pass: `erase-tlam`

Implementation: `dialects/src/tlam/EraseTlamPass.scala`

## What the pass does

It removes type-level lambda wrappers (`TLambda`) by splicing their bodies into the parent block and forwarding results from trailing `TReturn`.

Per `TLambda`:
1. move body ops except final `TReturn` before `TLambda`
2. replace `TLambda` result with `TReturn` value
3. erase `TLambda`

## Implementation walkthrough

`eraseInModule` recursively walks regions.

On `TLambda tl`:
1. read `bodyBlock = tl.body.blocks.head`
2. snapshot `bodyOps`
3. require `bodyOps.last` is `TReturn` (throws otherwise)
4. detach prefix ops (`bodyOps.dropRight(1).map(bodyBlock.detachOp)`)
5. insert detached ops before `tl` (`RewriteMethods.insertOpsBefore`)
6. replace op with no new ops and one forwarded result (`RewriteMethods.replaceOp` with `newResults = Some(Seq(tret.value))`)

Result forwarding handles RAUW for all uses of `tl.res`.

## Scope of this pass

- It erases type-level control (`TLambda`/`TReturn` structure).
- It does not lower value-level TLam ops (`VLambda`, `VApply`, `VReturn`); that is done by `lower-tlam-to-func`.

## Pipeline role

Typical order:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`

This keeps specialization semantics separate from structural erasure and backend lowering.
