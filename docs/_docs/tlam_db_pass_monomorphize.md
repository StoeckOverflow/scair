# Pass: `monomorphize`

File: `dialects/src/tlam_de_bruijn/Monomorphize.scala`

## Goal
Eliminate `tapply` by specializing polymorphic type-level lambdas (`tlambda`) at concrete type arguments.

This pass implements the DBI-only System F instantiation story for the
`tlam_de_bruijn` pipeline:
- type binders are represented only as `!tlam.bvar<k>` under `tlambda` / `forall`,
- instantiation is capture-avoiding substitution on DBI types,
- no SSA-in-types binder encoding is used to realize polymorphism.

## Core DBI discipline
Specialization uses `instAt(t, tyArg, depth)`:
- shift `tyArg` by current depth when needed,
- substitute at the correct De Bruijn level.

Implemented with:
- `DBI.shift`
- `DBI.subst`

Concretely, this is the System F reduction:
- `(Λα. t)[A]` becomes `t[α := A]`
- where `A` is first shifted to account for the current binder depth
  before substitution descends under nested `forall` / `tlambda`.

## High-level algorithm
1. Collect all `TLambda` definitions and `TApply` sites in module.
2. Iterate to fixed point:
   - For each `TApply(fun, tyArg)`:
     - Use its current `containerBlock` as the rewrite location.
     - If cached specialization `(block, fun, tyArg)` exists, reuse it.
     - Else, find `TLambda` producer and rewrite one site.
3. Leave unused lambda producers for the `dce` pass.

### High-level algorithm (detailed walkthrough)
The implementation is a rewrite-to-fixed-point loop over the module:

1. Build producer/consumer snapshots for the current iteration:
   - `collectTLambdas(mod)` returns a map from `Value[TypeAttribute]` to `TLambda`.
   - `collectTApplies(mod)` returns all current `TApply` operations.
2. Process each `TApply` in snapshot order:
   - Read `ta.containerBlock`.
   - If the op is detached (`containerBlock == None`), skip it.
   - If `ta.tyArg` is not a `TypeAttribute`, skip (verifier should catch invalid IR).
   - Compute cache key `(block, ta.fun, tyArg)`.
3. Cache behavior:
   - Cache hit: replace `ta` with no new ops and reuse the previously specialized value.
   - Cache miss: resolve producer of `ta.fun`:
     - If producer is a `TLambda`, call `rewriteOneTApply`.
     - Record rewritten result in cache and mark pass as changed.
4. Dead producer cleanup:
   - Monomorphization does not erase unused `TLambda` / `VLambda` producers.
   - Run `dce` after monomorphization when unused lambda producers should be removed.
5. Repeat while any rewrite occurred (`changed = true`).

Why fixed-point is needed:
- Rewrites can expose new opportunities (e.g., a newly cloned op graph creates additional cache hits or dead producers), so one linear pass is not sufficient.

What the cache guarantees:
- Within the same insertion block, identical `(fun, tyArg)` requests reuse one specialization result.
- This prevents redundant cloning for repeated identical `tapply` sites.
- The cache is block-local on purpose; the pass does not attempt cross-block
  dominance reasoning for specialization reuse.

## Rewriting one `TApply`
For matched `TLambda`:
1. Read lambda body block; require trailing `TReturn`.
2. Clone all body ops except `TReturn`, specializing types recursively.
3. Replace the `TApply` with those cloned ops.
4. Resolve mapped `TReturn` value.
5. Rewire the `TApply` result to the mapped return value.

### `TApply` rewriting (detailed mechanics)
Given `ta: TApply` and producer `tl: TLambda`, `rewriteOneTApply` performs:

1. Validate source lambda body shape:
   - Must have a body block.
   - Must be non-empty.
   - Last op must be `TReturn(v)`.
2. Initialize a fresh `valueMapper`:
   - Maps old SSA values (from lambda body) to cloned/new SSA values.
   - Used by recursive clone routines to remap operands/results consistently.
3. Clone non-terminator source ops:
   - `origOps.dropRight(1)` are cloned with `cloneOpSpec(op, ta.tyArg, depth = 0)`.
   - `cloneOpSpec` specializes every embedded type via `instAt`.
   - Region contents and nested ops are recursively cloned/remapped.
4. Compute replacement result:
   - Look up `retVal` from source `TReturn` in `valueMapper`.
   - This is the specialized value that semantically replaces `ta.res`.
5. Replace the original `ta`:
   - `RewriteMethods.replaceOp` inserts the cloned ops before `ta`,
   - rewires `ta.res` to the mapped return value,
   - and erases the original `ta`.

Why this preserves SSA correctness:
- Cloned defs are inserted before all rewritten uses at the same site.
- Users are rebuilt through the shared rewriter (`RewriteMethods.replaceOp` /
  `replaceValue`) rather than mutating operands in place.
- Value mapping ensures references inside cloned regions never point back to old defs.

Why this preserves type semantics:
- All type occurrences in cloned ops are specialized through `instAt`.
- `instAt` performs depth-aware DBI substitution (`shift` + `subst`), avoiding capture under nested binders.

## Cloning details
- Region/block/op cloning is recursive with a value mapper.
- Types are specialized in operands/results/region arg types.
- Entering `TLambda` increases specialization depth (`depth + 1`).
- This is still custom logic; generic `deepCopy` is not sufficient because it
  does not perform DBI-aware type instantiation.

## Preserved invariants
- Capture-avoid type substitution by construction (`shift` + `subst`).
- SSA use-def consistency via use replacement and local insertion.
- Region structural assumptions inherited from verifier.
- No free `!tlam.bvar<k>` should be introduced when eliminating the matched
  `tapply`; remaining DBI scoping is still enforced by verifier passes.

## Current limitations
- Some malformed-IR cases still use hard errors (`sys.error`) in internal helper paths.
- Dead specialized or polymorphic lambda producers are cleaned up by a later
  `dce` pass, not by monomorphization itself.
- If a `TApply` is detached by the time the iteration reaches it, the pass skips
  it rather than trying to rediscover a parent block.

## Operational notes
- The pass now uses the shared operation rewriter helpers (`RewriteMethods`) for
  `TApply` replacement instead of a local custom RAUW helper.
- That keeps insertion, SSA rewiring, and erasure consistent with the rest of
  the codebase.

## Audit-aligned notes
- The audit confirmed that this pass is the canonical DBI instantiation engine
  used by the lowering pipeline.
- The implementation does not rely on any `!value<%T>`-style binder rewriting.
- Nested binder depth handling and capture avoidance are now covered by direct
  unit tests in addition to filechecks.

## Relevant tests
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/monomorphize.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/nested_outer_binder_shift.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/reuse_three_sites.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/multi_tyargs.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/idempotence.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/03_monomorphize/top_level_tapply_no_crash.mlir`
- `dialects/test/src/TlamDeBruijnTypeParamsTest.scala`
