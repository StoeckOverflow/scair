# Pass: `erase-tlam`

File: `dialects/src/tlam_de_bruijn/EraseTlamPass.scala`

## Goal
Conservatively erase type-level TLam control after monomorphization:
- inline safe `TLambda` wrappers,
- remove the trailing `TReturn` only when that `TLambda` is inlined,
- splice body operations outward when doing so is known to be safe.

In the DBI-only pipeline, this pass is intentionally a structural cleanup pass,
not a polymorphism implementation pass. All type instantiation semantics must
already have been resolved by `monomorphize`.

This is not a strict normalization pass. If a `TLambda` is still semantically
meaningful, malformed, or contains forbidden residual type-level structure, the
pass may leave it in place.

## Rewrite strategy
For each `TLambda`:
1. Recursively erase nested `TLambda` first.
2. Read its single body block.
3. Check whether the last op is `TReturn`.
4. If the shape is valid and the body is type-safe to erase:
   detach all non-terminator body ops and insert them before the `TLambda`.
5. Replace the `TLambda` result with the `TReturn` operand.
6. Erase the `TLambda` op.
7. If the `TLambda` is not safe to inline:
   - erase it only when it has no users, or
   - otherwise leave it untouched.

### Rewrite strategy (detailed walkthrough)
The pass performs a region walk and handles `TLambda` nodes in a post-order style:

1. On each `TLambda`, it first rewrites nested `TLambda` operations in `tl.body`.
   - This guarantees inner type-level control is removed before outer wrappers are rewritten.
2. It then checks the lambda body shape.
3. If the body ends in `TReturn` and the body contains no forbidden residual
   type-level structure:
   - all body operations except the final `TReturn` are detached from the body block,
   - those detached operations are inserted directly before the `TLambda`,
   - the `TLambda` is replaced with no new operations and one new result:
     the operand carried by `TReturn`.
4. If the body is not safe to erase but the `TLambda` result is dead, the pass
   may still erase the wrapper.
5. If the body is not safe to erase and the result is still used, the pass
   leaves the `TLambda` in place.
6. Net effect:
   - safe type-level wrapper nodes disappear,
   - useful payload operations are preserved in original order when inlined,
   - unsafe or unresolved wrappers remain for later diagnostics.

Why this exact ordering matters:
- Rewriting inner lambdas first avoids dangling references if outer erasure depends on inner rewritten values.
- Moving payload ops before replacement ensures definitions dominate their subsequent uses.

### Why this pass exists separately from monomorphization
Although both passes touch type-level constructs, they solve different problems:

1. Different semantic responsibility
- `monomorphize` computes specialization semantics (`tapply` over `tlambda`) using DBI substitution.
- `erase-tlam` removes residual type-level control structure after specialization decisions are done.

2. Different correctness obligations
- `monomorphize` is about capture-avoiding type instantiation and specialization reuse.
- `erase-tlam` is about structural cleanup and SSA rewiring without changing specialization choices.

3. Better phase composability
- Keeping them separate allows pipeline tuning:
  - run analysis/simplification between specialization and erasure if needed,
  - test each phase boundary independently (which the FileCheck suite does).

4. Better debugging and regression isolation
- If specialization is wrong, failures show up before erasure.
- If structural cleanup is wrong, failures are localized to erasure/lowering stages.

5. Avoids over-coupled transforms
- A single “do everything” pass would mix type substitution, cloning, structural splicing, and lowering prep.
- Separation keeps each transform smaller and easier to reason about in proofs, audits, and tests.

## Preserved invariants
- When a `TLambda` is inlined, SSA value flow from its result is rewritten to
  the `TReturn` value.
- When a `TLambda` is inlined, body operations preserve original order.
- The pass never invents new specialization semantics; it only removes wrappers
  already proven unnecessary.
- Does not perform or depend on any SSA-in-types binder substitution.

## Current limitations
- It does not guarantee that all type-level TLam constructs are gone after the
  pass; unresolved wrappers can remain.
- Malformed `TLambda` is not force-rewritten; it is either skipped or removed
  only when dead.
- It does not itself verify DBI/region structure; relies on verifier/pipeline
  discipline.
- Erases type-level control only; value-level lowering is separate.

### Practical intuition
Think of the two-stage transition as:
- Stage A (`monomorphize`): decide *what* concrete type-level computation means.
- Stage B (`erase-tlam`): remove type-level scaffolding now that concrete meaning is fixed.

This separation is especially useful in a thesis context because it aligns with standard compiler phase design: semantic resolution first, representation erasure second.

## Relevant tests
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/erase.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/no_leftovers.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/strict_leftovers_extra.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/unused_poly_cleanup.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/guard_uninstantiated.mlir`
