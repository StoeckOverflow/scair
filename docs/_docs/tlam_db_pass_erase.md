# Pass: `erase-tlam`

File: `dialects/src/tlam_de_bruijn/EraseTlamPass.scala`

## Goal
Erase type-level TLam control after monomorphization:
- remove `TLambda` wrappers,
- remove `TReturn` terminators,
- splice value-level body operations outward.

## Rewrite strategy
For each `TLambda`:
1. Recursively erase nested `TLambda` first.
2. Read its single body block.
3. Require last op to be `TReturn`.
4. Detach all non-terminator body ops and insert before the `TLambda`.
5. Replace `TLambda` result with the `TReturn` operand.
6. Erase `TLambda` op.

### Rewrite strategy (detailed walkthrough)
The pass performs a region walk and handles `TLambda` nodes in a post-order style:

1. On each `TLambda`, it first rewrites nested `TLambda` operations in `tl.body`.
   - This guarantees inner type-level control is removed before outer wrappers are rewritten.
2. It then inspects the lambda body block and snapshots its operations.
3. The final operation must be `TReturn`; otherwise the pass throws.
4. All body operations except the final `TReturn` are detached from the body block.
5. Those detached operations are inserted directly before the `TLambda` in the parent block.
6. The `TLambda` operation is replaced with no new operations and one new result:
   - the operand carried by `TReturn`.
7. Net effect:
   - type-level wrapper nodes disappear,
   - useful payload operations are preserved in original order,
   - users of the old `TLambda` result now use the returned value.

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
- SSA value flow from `TLambda` result is rewritten to `TReturn` value.
- Body operations preserve original order.
- Works best when input already satisfies verifier invariants.

## Current limitations
- Assumes structural validity for intended rewrite path; malformed `TLambda` is skipped and left for verifier diagnostics.
- Does not itself verify DBI/region structure; relies on verifier/pipeline discipline.
- Erases type-level control only; value-level lowering is separate.

## Recent hardening update
- Implementation now handles malformed `TLambda` bodies more robustly:
  - if trailing `TReturn` is missing, `erase-tlam` leaves that op unchanged instead of throwing.
- Rationale:
  - prevents pass-level crash on invalid input,
  - allows verifier diagnostics to surface structural issues cleanly.
- Effect:
  - valid, staged pipelines are unchanged,
  - invalid IR is handled gracefully.

## Recommended usage
Run after `monomorphize`, before `lower-tlam-to-func`.

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
