---
title: "TLam Pass: monomorphize"
---

# Pass: `monomorphize`

Implementation: `dialects/src/tlam/Monomorphize.scala`

## What this pass does

`monomorphize` eliminates `tlam.tapply` by cloning the referenced `tlam.tlambda` body at each use site and specializing all types with the concrete `tyArg`.

Think of it as:
1. find `%spec = tlam.tapply %poly <tyArg = T>`
2. inline the type-level body of `%poly` right before `%spec`
3. rewrite every type in that clone as if the type parameter were `T`
4. replace `%spec` with the cloned return value
5. erase the original `tlam.tapply`

It does **not** erase remaining `tlam.tlambda` structure in general; that is the role of `erase-tlam`.

## Why this pass exists

Later passes should not need to reason about open type-level application (`tapply`).
After monomorphization, each use site has a concrete specialized version, which simplifies:
- `erase-tlam`
- `lower-tlam-to-func`
- canonicalization/CSE on specialized IR

## Core idea: two substitutions happen together

The helper `inst(t, binderOpt, tyArg)` applies both:
1. de Bruijn substitution for `!tlam.bvar<0>` via `DBI.subst(0, tyArg, t)`
2. SSA type-variable substitution for `!tlam.tvar<%binder>` via `substTVar(...)`

This is the key TLam detail: specialization must handle both binding encodings in one rewrite.

## Rewrite algorithm (module-level)

`run(mod)` executes a fixed-point loop:
1. collect `TLambda` producers (`collectTLambdas`)
2. collect `TApply` users (`collectTApplies`)
3. for each `TApply`:
   - if `(block, fun, tyArg)` already specialized, reuse cached value and erase duplicate apply
   - else, if `fun` resolves to a known `TLambda`, rewrite it via `rewriteOneTApply`
4. if a rewritten `TLambda` result has no uses left, erase that `TLambda`
5. repeat while changes happen

Cache key: `(use block, callee value, tyArg)`.
So identical specializations in the same block are deduplicated.

## Rewrite of one `tapply`

`rewriteOneTApply(ta, tl)`:
1. read tlambda body block and require trailing `tlam.treturn`
2. read optional binder block argument (`%T : !tlam.type`) for `tvar` substitution
3. clone all body ops except final `treturn` with type specialization (`cloneOpSpec`)
4. insert cloned ops immediately before the `tapply`
5. map original return value to its cloned value
6. replace all uses of `ta.res` with cloned return (`replaceAllUsesWith`)
7. erase `ta`

If the shape is malformed (missing body/return/mapping), the rewrite bails out for that use.

## Important implementation detail: replacing value uses in types

`replaceAllUsesWith(from, to)` handles two channels:
1. normal SSA operand uses (`from.uses`)
2. embedded value uses in attributes/types (`from.typeUses`)

The second part is essential in SSA-in-types mode, where values can be referenced inside type attributes (for example `!tlam.tvar<%T>`).

## Cloning behavior

`cloneOpSpec` has explicit handling for TLam ops:
- `VLambda`, `VReturn`, `VApply`
- `TLambda`, `TReturn`, `TApply`

For each cloned op it:
1. remaps operands through `valueMapper`
2. specializes all `TypeAttribute` positions via `inst(...)`
3. clones nested regions recursively
4. records old-result -> new-result mapping

Other ops are cloned through a generic `updated(...)` fallback with the same specialization/remapping rules.

## Example: repeated specialization is deduplicated

Input shape:
```mlir
%mk = "tlam.tlambda"() ({
^bb0(%T: !tlam.type):
  %v = "test.mk_poly"() : () -> !tlam.forall<!tlam.tvar<%T>>
  "tlam.treturn"(%v) : (!tlam.forall<!tlam.tvar<%T>>) -> ()
}) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

%s0 = "tlam.tapply"(%mk) <{tyArg = i32}> ... -> !tlam.forall<i32>
%s1 = "tlam.tapply"(%mk) <{tyArg = i32}> ... -> !tlam.forall<i32>
```

After `monomorphize`:
- both `tapply` ops are gone
- only one specialized clone is materialized in that block
- both users point to that specialized result

See: `tests/filecheck/dialects/tlam/06_monomorphize/tlam_monomorphize_ssa_and_dbi.mlir`.

## Example: tyArg containing `tvar` is preserved

The pass correctly specializes even when `tyArg` itself contains `!tlam.tvar<%Y>`.
It performs DBI substitution and preserves SSA identity of `%Y` inside the resulting type.

See: `tests/filecheck/dialects/tlam/06_monomorphize/tlam_ssa_monomorphize_tvar_in_tyarg.mlir`.

## Pipeline position

Typical ordering:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`

Common full pipeline variants in tests:
- `beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,...`
- `canonicalize,monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func,...`

## Failure/robustness expectations

- User-facing structural/type mismatches are expected to be reported by verifiers (for example malformed `tapply` result types).
- Internal impossible states during cloning currently use `sys.error(...)` with `monomorphize:` diagnostics.
