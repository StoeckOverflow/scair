---
title: "TLam Pass: monomorphize"
---

# Pass: `monomorphize`

Implementation: `dialects/src/tlam/Monomorphize.scala`

## What the pass does

It rewrites `tlam.tapply` by specializing the referenced `tlam.tlambda` body at the concrete type argument.

Main effect:
1. clone lambda body operations at the `tapply` use site
2. substitute type binders in cloned types
3. replace `tapply` result with specialized cloned return value
4. erase `tapply`

## Substitution model

The pass applies two substitutions in `inst(...)`:
1. de Bruijn substitution for `bvar`: `DBI.subst(0, tyArg, t)`
2. SSA binder substitution for `tvar`: `substTVar(t1, binder, tyArg)` for `!tlam.tvar<%binder>`

This is the key distinction of the SSA-in-types dialect: DBI and SSA-type-var substitution run together.

## Core algorithm

`run(mod)` executes fixed-point rewriting:
1. collect all `TLambda` producers by result value (`collectTLambdas`)
2. collect all `TApply` users (`collectTApplies`)
3. for each `TApply`:
   - if `(block, fun, tyArg)` exists in cache, reuse prior specialization result and erase duplicate apply
   - else if `fun` resolves to a known `TLambda`, rewrite one apply via `rewriteOneTApply`
4. if a `TLambda` result has no remaining uses, erase the `TLambda`

Cache purpose: deduplicate identical specialization requests within the same use block.

## How one `TApply` is rewritten

`rewriteOneTApply(ta, tl)`:
1. validate source lambda body exists and ends with `TReturn`
2. read optional SSA binder (`origBlock.arguments.headOption`)
3. clone all body ops except final `TReturn` with specialization (`cloneOpSpec`)
4. insert cloned ops before `ta`
5. resolve mapped return value from `valueMapper`
6. replace all uses of `ta.res` with mapped value (`replaceAllUsesWith`)
7. erase `ta`

## Cloning and remapping details

`cloneOpSpec` handles TLam ops explicitly (`VLambda`, `VReturn`, `VApply`, `TLambda`, `TReturn`, `TApply`) and has a generic fallback for other ops.

For each cloned op it:
- remaps operands through `valueMapper`
- specializes all type positions via `inst(...)`
- clones nested regions recursively
- records old->new result mapping in `valueMapper`

## Use replacement details

`replaceAllUsesWith(from, to)` rewrites both:
1. regular operand uses (`from.uses`) by rebuilding user ops
2. embedded value uses in attributes/types (`from.typeUses`) by rewriting value attributes and re-registering `typeUses`

This is required so `tvar` references in types remain consistent after specialization.

## Notable implementation behavior

The pass uses `sys.error(...)` for unexpected internal malformed states (missing body block, wrong terminator, missing clone mapping). Normal user-facing IR errors should be caught earlier by verifier checks.
