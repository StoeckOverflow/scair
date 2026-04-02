---
title: "TLam Pass: monomorphize"
---

# Pass: `monomorphize`

Implementation: `dialects/src/tlam/Monomorphize.scala`

Pass entrypoints:
- `scair.passes.Monomorphize.run(mod: ModuleOp)`
- `scair.passes.MonomorphizePass`

## What this pass does

`monomorphize` removes `tlam.tapply` by specializing the referenced
`tlam.tlambda` body for the concrete `tyArg` at each use site.

For a use like `%spec = tlam.tapply %poly <tyArg = T>`, the pass:
1. resolves `%poly` to a defining `tlam.tlambda`
2. clones the `tlambda` body operations before `%spec`
3. rewrites the cloned IR as if the bound type variable were `T`
4. replaces `%spec` with the cloned `treturn` value
5. erases the original `tlam.tapply`

The pass does not erase all `tlam.tlambda` operations unconditionally. It only
erases a `tlambda` when its result becomes unused after rewriting. Remaining
TLam structure is handled later by `erase-tlam`.

## Why this pass exists

Later lowering passes should not have to reason about open type-level
application. After monomorphization, the IR contains concrete specializations
instead of unresolved `tlam.tapply`, which simplifies:
- `erase-tlam`
- `lower-tlam-to-func`
- canonicalization and CSE on already-specialized IR

## Required input shape

A `tapply` is only rewritten when its callee resolves to a clonable
`tlam.tlambda` body:
1. the `tlambda` must have a body region with at least one block
2. the first block must end in `tlam.treturn`
3. the block may have an optional binder argument such as `%T: !tlam.type`
4. the body prefix before `treturn` must be cloneable into the use site

If any `tapply` remains after the pass reaches a fixed point, the pass fails
with:

```text
monomorphize: unresolved tapply remained; callee must resolve to a clonable tlam.tlambda body
```

## Core specialization rule

The key helper is:

```scala
inst(t, binderOpt, tyArg): TypeAttribute
```

It applies two substitutions in sequence:
1. de Bruijn substitution via `DBI.subst(0, tyArg, t)`
2. SSA type-variable substitution via `substTVar(...)`

This is the core implementation detail: the pass supports both TLam binding
encodings and rewrites them together during specialization.

### De Bruijn substitution

`DBI.subst(0, tyArg, t)` replaces the innermost bound type variable represented
as `!tlam.bvar<0>`.

### SSA type-variable substitution

If the `tlambda` body block has a binder argument such as `%T: !tlam.type`, then
`substTVar` also replaces occurrences of `!value<%T>` inside type attributes
with the concrete `tyArg`.

The implementation handles these cases explicitly:
- `ValueRefType` pointing at the binder is replaced with a cloned `tyArg`
- other `ValueRefType` nodes are rebuilt so the SSA reference is preserved
- `TlamFunType` and `TlamForAllType` are rewritten recursively
- generic `ParametrizedAttribute` payloads are rebuilt recursively

## Attribute and payload rewriting

Specialization is not limited to top-level result types. The implementation
rewrites TLam types anywhere they appear in cloned IR:
- result types
- block argument types
- operation properties
- operation attributes
- nested payloads of `ParametrizedAttribute`

The relevant helpers are:
- `instPayload`
- `instAttr`
- `instAndRemapAttr`
- `rebuildAttr`
- `cloneAttr`

`rebuildAttr` reconstructs product-style attributes with rewritten payloads, so
the pass can preserve the original attribute class while specializing the data
stored inside it.

## Value remapping inside types

The pass must preserve SSA identity even when values appear inside types.

Two mechanisms make that work:
1. `valueMapper` tracks old SSA values to cloned replacements
2. `AttributeWalker.remapTypeUsesInPlace(...)` updates embedded value references
   inside specialized attributes

Without the second step, a cloned attribute could still reference the original
SSA definitions rather than the specialized clone.

## Rewrite algorithm (module-level)

`run(mod)` executes a fixed-point rewrite over the module:
1. collect all `TLambda` producers with `collectTLambdas`
2. collect all `TApply` users with `collectTApplies`
3. for each `TApply`, resolve its callee and try to rewrite it
4. erase a `TLambda` if its result has no remaining uses
5. repeat until an iteration makes no changes
6. fail if any `tlam.tapply` still remains

The collectors are recomputed on every iteration rather than updated
incrementally. That keeps the rewrite simple and makes nested region rewrites
safe.

### Specialization cache

The pass memoizes reusable specializations in:

```scala
mutable.Map[(Block, Value[TlamForAllType], TypeAttribute), Value[TypeAttribute]]
```

The cache key is:
- the use block
- the callee value
- the concrete `tyArg`

So repeated specializations are deduplicated only within the same block. The
same `tapply` shape in a different block is specialized separately.

## Cacheability and effect checks

The pass only reuses a previous specialization when the source `tlambda` body
prefix is safe to share.

`tlambdaPrefixIsEffectFree(tlam)` requires:
1. the body block exists
2. the last op is `tlam.treturn`
3. every op before that satisfies `isEffectFreeForSpecialization`

`isEffectFreeForSpecialization` currently treats these as shareable:
- any op implementing `NoMemoryEffect`
- `VLambda`
- `TLambda`
- `TApply`

If the prefix is not effect-free, the pass still monomorphizes the `tapply`, but
it clones a fresh specialization for each use and does not insert it into the
cache.

## Rewrite of one `tapply`

`rewriteOneTApply(ta, tl)` performs the single-site rewrite:
1. read the first body block of the source `tlambda`
2. read the optional binder block argument
3. require the final op to be `tlam.treturn`
4. create fresh `blockMapper` and `valueMapper`
5. validate whether binder SSA operand use is supported
6. deep-copy every op before the final `treturn`
7. specialize each detached clone with `specializeOpInPlace(...)`
8. insert the specialized clones immediately before the `tapply`
9. look up the cloned replacement for the original returned value
10. replace all uses of `ta.res`
11. erase `ta`

If the body is missing, malformed, or unsupported for a particular use, the
function returns `None` and leaves the `tapply` in place for that iteration.

### Binder used as a term SSA operand

The block binder can be used either inside types (`!value<%T>`) or as a normal
SSA operand.

If the binder is used as a term SSA operand, specialization is only supported
when `ta.tyArg` is itself a `ValueRefType`, for example `!value<%X>`. In that
case the pass seeds:

```scala
binder -> tv.value
```

into `valueMapper`, so cloned operand uses of `%T` become operand uses of `%X`.

If the binder is used as a term operand and `tyArg` is not a `ValueRefType`, the
rewrite refuses that `tapply`. This is the `unsupportedBinderOperandUse` check.

## Cloning and specialization internals

The pass separates cloning from specialization:
1. `deepCopy(using blockMapper, valueMapper)` duplicates the original op tree
2. `specializeOpInPlace(...)` rebuilds that clone with specialized types and
   remapped references

`specializeOpInPlace` has explicit handling for:
- `VLambda`
- `VReturn`
- `VApply`
- `TLambda`
- `TReturn`
- `TApply`

These explicit cases are necessary because region-carrying operations such as
`VLambda` and `TLambda` cannot rely on a generic `updated(...)` path once the
regions themselves need to be rebuilt with specialized block arguments and
remapped values.

For all other operations, the pass uses `other.updated(...)` after constructing:
- specialized results
- specialized nested regions
- specialized properties
- specialized attributes
- remapped operands
- remapped successors

### Region cloning

Nested regions are rebuilt by `specializeRegion(...)`.

For each original block, the pass:
1. specializes every block argument type
2. creates a new empty block with those argument types
3. records `oldBlock -> newBlock` in `blockMapper`
4. records `oldArg -> newArg` in `valueMapper`
5. specializes every operation in the block
6. assembles a new `Region` from the rebuilt blocks

The helper `rebindMappedValue(...)` ensures that if a value already has aliases
in `valueMapper`, those aliases are retargeted to the newest replacement value.

## Replacing the `tapply` result

Once the specialized return value is found, the pass replaces the original
`tapply` result with:

```scala
RewriteMethods.replaceValue(ta.res, newRet)
```

This replacement must handle both:
1. normal SSA operand uses
2. embedded uses inside types and attributes

That second channel is essential in SSA-in-types mode.

## Example: repeated specialization is deduplicated

Input shape:

```mlir
%mk = "tlam.tlambda"() ({
^bb0(%T: !tlam.type):
  %v = "test.mk_poly"() : () -> !tlam.forall<!value<%T>>
  "tlam.treturn"(%v) : (!tlam.forall<!value<%T>>) -> ()
}) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

%s0 = "tlam.tapply"(%mk) <{tyArg = i32}> ... -> !tlam.forall<i32>
%s1 = "tlam.tapply"(%mk) <{tyArg = i32}> ... -> !tlam.forall<i32>
```

After `monomorphize`:
- both `tapply` ops are gone
- one specialized clone is materialized in that block when the `tlambda` prefix
  is effect-free
- both users point to that same specialized result

See:
`tests/filecheck/dialects/tlam/06_monomorphize/tlam_monomorphize_ssa_and_dbi.mlir`.

## Example: `tyArg` containing embedded SSA refs is preserved

The pass correctly specializes even when `tyArg` itself contains `!value<%Y>`
inside nested TLam type structure. It performs de Bruijn substitution and
preserves the SSA identity of `%Y` inside the resulting type.

See:
`tests/filecheck/dialects/tlam/06_monomorphize/tlam_ssa_monomorphize_tvar_in_tyarg.mlir`.

## Example: nested attribute payloads are rewritten too

The pass also specializes TLam types found inside:
- op attributes
- op properties
- nested parametrized attributes

So a body containing payloads like:

```mlir
"test.use"(%tv)
  {dep = !tlam.forall<!tlam.fun<!value<%tv>, !tlam.forall<!value<%T>>>>}
  : (!tlam.type) -> ()
```

is rewritten so:
- `%T` becomes the concrete `tyArg`
- `%tv` inside the attribute stays tied to the cloned `%tv`, not the original
  definition

See:
`tests/filecheck/dialects/tlam/06_monomorphize/tlam_monomorphize_ssa_and_dbi.mlir`.

## Pipeline position

Typical ordering:
1. `monomorphize`
2. `erase-tlam`
3. `lower-tlam-to-func`

Common full pipeline variants in tests:
- `beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,...`
- `canonicalize,monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func,...`

## Failure and robustness notes

- Structural or type mismatches should usually be caught by verifiers first.
- A `tlambda` that directly returns its binder (`treturn %T`) is rejected by the
  TLam verifier before monomorphization.
- A `tapply` that cannot be rewritten is a hard pass failure, not a silent
  no-op.
- Internal impossible states during specialization currently use
  `sys.error(...)` with `monomorphize:` diagnostics.

## Practical reading guide

If you are inspecting the implementation, these are the key methods:
1. `run` for the fixed-point driver and cache
2. `rewriteOneTApply` for the single-site rewrite
3. `inst` and `substTVar` for substitution semantics
4. `specializeOpInPlace` and `specializeRegion` for rebuilding cloned IR
