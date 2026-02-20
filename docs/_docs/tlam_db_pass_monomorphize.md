# Pass: `monomorphize`

File: `dialects/src/tlam_de_bruijn/Monomorphize.scala`

## Goal
Eliminate `tapply` by specializing polymorphic type-level lambdas (`tlambda`) at concrete type arguments.

## Core DBI discipline
Specialization uses `instAt(t, tyArg, depth)`:
- shift `tyArg` by current depth when needed,
- substitute at the correct De Bruijn level.

Implemented with:
- `DBI.shift`
- `DBI.subst`

## High-level algorithm
1. Collect all `TLambda` definitions and `TApply` sites in module.
2. Iterate to fixed point:
   - For each `TApply(fun, tyArg)` with container block:
     - If cached specialization `(block, fun, tyArg)` exists, reuse it.
     - Else, find `TLambda` producer and rewrite one site.
3. Optional cleanup: erase `TLambda` when unused.

### High-level algorithm (detailed walkthrough)
The implementation is a rewrite-to-fixed-point loop over the module:

1. Build producer/consumer snapshots for the current iteration:
   - `collectTLambdas(mod)` returns a map from `Value[TypeAttribute]` to `TLambda`.
   - `collectTApplies(mod)` returns all current `TApply` operations.
2. Process each `TApply` in snapshot order:
   - If `ta.containerBlock` is absent (module-scope corner case), skip it.
   - If `ta.tyArg` is not a `TypeAttribute`, skip (verifier should catch invalid IR).
   - Compute cache key `(block, ta.fun, tyArg)`.
3. Cache behavior:
   - Cache hit: replace all uses of `ta.res` with previously specialized value, erase `ta`.
   - Cache miss: resolve producer of `ta.fun`:
     - If producer is a `TLambda`, call `rewriteOneTApply`.
     - Record rewritten result in cache and mark pass as changed.
4. Producer cleanup:
   - After rewriting a site, if producer `TLambda` result has no uses, erase it.
5. Repeat while any rewrite occurred (`changed = true`).

Why fixed-point is needed:
- Rewrites can expose new opportunities (e.g., a newly cloned op graph creates additional cache hits or dead producers), so one linear pass is not sufficient.

What the cache guarantees:
- Within the same insertion block, identical `(fun, tyArg)` requests reuse one specialization result.
- This prevents redundant cloning for repeated identical `tapply` sites.

## Rewriting one `TApply`
For matched `TLambda`:
1. Read lambda body block; require trailing `TReturn`.
2. Clone all body ops except `TReturn`, specializing types recursively.
3. Insert cloned ops before `TApply` site.
4. Resolve mapped `TReturn` value.
5. Replace all uses of `TApply` result with mapped return value.
6. Erase original `TApply`.

### `TApply` rewriting (detailed mechanics)
Given `ta: TApply` and producer `tl: TLambda`, `rewriteOneTApply` performs:

1. Validate source lambda body shape:
   - Must have a body block.
   - Must be non-empty.
   - Last op must be `TReturn(v)`.
2. Determine insertion location:
   - `useBlock = ta.containerBlock` (must exist in this rewrite path).
   - Cloned operations are inserted immediately before `ta` to preserve dominance.
3. Initialize a fresh `valueMapper`:
   - Maps old SSA values (from lambda body) to cloned/new SSA values.
   - Used by recursive clone routines to remap operands/results consistently.
4. Clone non-terminator source ops:
   - `origOps.dropRight(1)` are cloned with `cloneOpSpec(op, ta.tyArg, depth = 0)`.
   - `cloneOpSpec` specializes every embedded type via `instAt`.
   - Region contents and nested ops are recursively cloned/remapped.
5. Materialize clones:
   - Insert cloned ops in original order before `ta`.
6. Compute replacement result:
   - Look up `retVal` from source `TReturn` in `valueMapper`.
   - This is the specialized value that semantically replaces `ta.res`.
7. Rewrite uses and erase call:
   - Replace all uses of `ta.res` with mapped return value.
   - Erase original `ta`.

Why this preserves SSA correctness:
- Cloned defs are inserted before all rewritten uses at the same site.
- Users are rebuilt with updated operands (`replaceAllUsesWith`) instead of mutating in-place.
- Value mapping ensures references inside cloned regions never point back to old defs.

Why this preserves type semantics:
- All type occurrences in cloned ops are specialized through `instAt`.
- `instAt` performs depth-aware DBI substitution (`shift` + `subst`), avoiding capture under nested binders.

## Cloning details
- Region/block/op cloning is recursive with a value mapper.
- Types are specialized in operands/results/region arg types.
- Entering `TLambda` increases specialization depth (`depth + 1`).

## Preserved invariants
- Capture-avoid type substitution by construction (`shift` + `subst`).
- SSA use-def consistency via use replacement and local insertion.
- Region structural assumptions inherited from verifier.

## Current limitations
- Module-scope `TApply` has no container block; pass now skips it (no crash), no rewrite.
- Some malformed-IR cases still use hard errors (`sys.error`) in internal helper paths.
- No global dead-specialization elimination pass beyond existing cleanup opportunities.

## Relevant tests
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/monomorphize.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/binder_shift.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/nested_outer_binder_shift.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/reuse_three_sites.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/multi_tyargs.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/idempotence.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/top_level_tapply_no_crash.mlir`
