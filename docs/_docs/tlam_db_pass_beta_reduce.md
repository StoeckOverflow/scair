# Pass: `beta-reduce-tlam`

File: `dialects/src/tlam_de_bruijn/BetaReduceTLamPass.scala`

## Goal
Perform conservative value-level beta reduction:
`vapply(vlambda, arg) -> inline(vlambda.body, x := arg)`

This is value-level beta reduction only (not type instantiation).
It is optional in the DBI-only pipeline and is independent of DBI type
substitution semantics.

## Matching rule
A `VApply` is considered only when:
- `app.fun.owner` is directly a `VLambda`.
- Lambda has reducible shape:
  - exactly one block,
  - exactly one block argument,
  - last op is `VReturn`.

## Safety policy
Reduction is skipped unless all hold:
- All non-terminator ops in lambda body are recursively `NoMemoryEffect`.
- If the argument value is effectful, lambda parameter must not be used more than once.

This avoids effect duplication and non-local semantic changes.

## Rewrite algorithm
1. Read lambda body ops and split into:
   - non-terminator ops to clone,
   - return value from `VReturn`.
2. Initialize value mapping with block argument -> `app.arg`.
3. Deep-copy body ops and insert before `VApply`.
4. Resolve mapped return value.
5. Replace all uses of `VApply` result with mapped return value.
6. Erase original `VApply`.

### Rewrite algorithm (detailed walkthrough)
The implementation follows these concrete steps for each candidate `VApply`:

1. Candidate discovery
- Module walk recursively visits regions/blocks and checks each op.
- `VApply` nodes are tested by `tryReduce`.

2. Producer and shape validation
- `app.fun.owner` must be a `VLambda`.
- Lambda region must satisfy reducible shape:
  - one block,
  - one block argument,
  - final operation is `VReturn`.

3. Body decomposition
- `nonTermOps = bodyOps.dropRight(1)` (ops to clone).
- `ret = bodyOps.last.asInstanceOf[VReturn]`.
- `blockArg = lamBlock.arguments.head`.

4. Safety gating
- `nonTermOps` must be recursively pure (`isPureRec`).
- If `app.arg` is effectful (`isEffectfulValue`) and parameter use count > 1 (`countUsesInLambda`), skip.

5. Value mapping and cloning
- Start mapper with `blockArg -> app.arg`.
- Deep-copy each non-terminator op; cloned defs are tracked by mapper.

6. Materialization and use rewrite
- Insert clones immediately before `app`.
- Compute mapped return value from mapper (fallback to original if unchanged).
- Replace all uses of `app.res` with mapped return value by rebuilding user ops.
- Erase `app`.

7. No-op behavior
- If any guard fails, pass leaves the operation unchanged.
- If `app` has no container block, pass also leaves it unchanged.

## SSA and dominance strategy
- Uses are replaced by rebuilding user ops (via `updated(...)`) and replacing old users.
- Cloned defs are inserted before the original `VApply`, so they dominate rewritten uses.

### Why this is capture-safe for values
This pass does not perform textual substitution. Instead:
- it clones operations with IR-level value mapping,
- maps the lambda parameter to the actual argument value,
- remaps all internal references through the mapper.

As a result, nested regions keep their own binders and no accidental variable capture is introduced by string-level replacement.

### Why effect guards are needed
Without guards, beta reduction can change semantics:
- If lambda body contains side effects, inlining may duplicate/reorder effects.
- If argument production is effectful and parameter is used multiple times, naive substitution duplicates effectful computation.

The current policy intentionally refuses such reductions unless it is clearly safe.

## Preserved invariants
- Region terminators are not structurally rewritten in lambda definitions.
- No type-level DBI rewriting is performed.
- Conservative purity checks avoid effectful miscompilations.
- The pass does not participate in `tapply` / `tlambda` instantiation and does
  not rely on any type-binder encoding beyond the existing verified IR.

### What is intentionally *not* preserved/attempted
- No guarantee of maximal beta-normal form in one run.
- No cross-block/global scheduling optimization after rewrite.
- No interprocedural reasoning about equivalent pure computations.

## Limitations
- No global fixpoint across new opportunities inside rewritten code beyond current walk behavior.
- No advanced alias/effect analysis; purity is trait-based and conservative.
- Does not reduce when callee is indirect (e.g., block argument / unknown producer).

### Practical implications of limitations
- Some reducible patterns remain by design to avoid unsafe transformations.
- Reduction opportunities may depend on running additional canonicalization/CSE passes afterward.
- For aggressive partial evaluation, a richer effect model would be required.

### Why this pass is separate from monomorphization
- `beta-reduce-tlam` is value-level and effect-sensitive.
- `monomorphize` is type-level and DBI-substitution-driven.
- Keeping them separate prevents mixing value side-effect policy with type specialization logic, which simplifies reasoning and testing.

## Recommended pipeline placement
If used, run this pass before `monomorphize`:
- `beta-reduce-tlam -> monomorphize -> erase-tlam -> lower-tlam-to-func`

This keeps value-level inlining separate from type-level instantiation and makes
later pass behavior easier to reason about.

## Relevant tests
- `tests/filecheck/dialects/tlam_de_bruijn/06_beta/beta.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/06_beta/safety_edges.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/06_beta/shadowing_and_dup.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/06_beta/nested_apply_chain.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/06_beta/block_arg_callee_no_reduce.mlir`
