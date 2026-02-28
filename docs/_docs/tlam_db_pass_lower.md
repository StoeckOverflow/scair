# Pass: `lower-tlam-to-func`

File: `dialects/src/tlam_de_bruijn/LowerTlamToFuncPass.scala`

## Goal
Lower remaining value-level TLam constructs into `func` dialect:
- `vlambda` -> lifted `func.func` + `func.constant` symbol value
- `vapply`  -> `func.call_indirect`
- `vreturn` -> `func.return`

This pass assumes the DBI-only type-level pipeline is already complete:
- `tapply` has been instantiated away by `monomorphize`,
- type-level wrappers have been cleaned by `erase-tlam`,
- only value-level execution constructs remain to be lowered.

## Phase 1: lambda lifting
For each `VLambda`:
1. Create a fresh symbol name (`lifted_N`).
2. Convert TLam function type to builtin `FunctionType`.
3. Move lambda body region into new `func.func`.
4. Insert `func.func` at module top.
5. Materialize function value via `func.constant @lifted_N`.
6. Replace uses of original `VLambda` result with constant result.
7. Erase original `VLambda`.

### Phase 1 details (what actually happens)
The pass walks module regions and snapshots operations before rewriting. For each `VLambda`:

1. Name generation
- A monotonically increasing counter builds unique symbols (`lifted_1`, `lifted_2`, ...).

2. Type lowering
- TLam function type `tlam.fun<in,out>` is translated to builtin `FunctionType(inputs=[in], outputs=[out])`.
- This is required because `func.call_indirect` expects builtin function types.

3. Body transfer
- Lambda body region is detached/moved into the newly created `func.func`.
- The original `VLambda` is erased afterward, so ownership remains consistent.

4. Function value materialization
- A first-class function value is created with `func.constant @lifted_N`.
- This value replaces the old `VLambda` result in users.

5. SSA rewiring
- The synthesized `func.constant` is inserted with block-aware rewrite helpers,
  not by mutating the raw operation list.
- Uses are then rewritten through the core `replaceValue` helper.
- This keeps parent links, result ownership, and use-def chains valid in this
  IR framework.

## Phase 2: op rewriting
Pattern-rewrite remaining ops:
- `VApply` -> `CallIndirect` with callee/result types from builtin `FunctionType`.
- `VReturn` -> `Return`.

### Phase 2 details
After lambda lifting, value-level TLam ops are lowered by pattern rewriting:

1. `VApply` lowering
- Reads callee operand and checks runtime type is builtin `FunctionType`.
- Creates `func.call_indirect` with:
  - callee function value,
  - argument list from `VApply` argument,
  - result types from callee function outputs.

2. `VReturn` lowering
- Replaced directly with `func.return` carrying the same returned value.

3. Greedy application
- Rewrites are run through a greedy pattern walker over the module.
- This allows chained `VApply`/`VReturn` sites to normalize in one lowering phase.

## Preserved invariants
- SSA use-def rewiring via explicit replacement helper.
- Function bodies remain explicit regions with `func.return` terminators.
- All value-level TLam ops removed when lowering is complete.

### Why this preserves correctness
- Semantic equivalence of function values:
  - `VLambda` value is replaced by symbol constant referencing lifted function body.
- Call equivalence:
  - `VApply(fun,arg)` becomes `call_indirect(fun,arg)` with matching function signature.
- Return equivalence:
  - `VReturn(v)` maps 1:1 to `func.return(v)`.

### Dominance and ordering considerations
- Lifted `func.func` ops are inserted at module top.
- `func.constant` values are inserted in place of original lambda sites.
- Replacing users after insertion ensures all uses see valid dominating defs.

This matters in practice: an earlier version inserted `func.constant` by
mutating the block op list directly, which left the op textually present but not
properly attached to its parent block for verifier-side dominance checks. The
current implementation uses rewrite helpers specifically to avoid that class of
bug.

## Current limitations
- Expects lowered `VApply` callee values to already have builtin `FunctionType`.
- Throws exception if unexpected callee type remains at rewrite time.
- Type-level TLam ops should be erased beforehand (`erase-tlam`).

## Recent hardening update
- Added explicit precondition guard in implementation:
  - if type-level TLam control ops (`TLambda`, `TApply`, `TReturn`) are still present,
    `lower-tlam-to-func` returns early without rewriting.
- Rationale:
  - enforces phase contract (`erase-tlam` before lowering),
  - avoids partial/unsafe lowering in mis-staged pipelines.
- Effect:
  - valid pipelines unchanged,
  - mis-ordered pipelines fail cleanly at verification/staging boundaries.

### Practical implications
- If `erase-tlam` has not run (or did not fully normalize type-level constructs), lowering now no-ops at pass entry.
- If some callee path still carries TLam type instead of builtin `FunctionType`, lowering throws by design.
- The pass intentionally prefers explicit failure over silent mis-lowering.

## Audit-aligned runtime note
The lowered IR is expected to run on the current interpreter using:
- `func.constant` for first-class function values,
- `func.call_indirect` for indirect application,
- `func.return` for function returns.

The end-to-end DBI pipeline regression now exercises the normal verifier path
without requiring `scair-opt -s`.

### Why lowering is separate from erasure/monomorphize
- `monomorphize` resolves type-level polymorphism semantics.
- `erase-tlam` removes type-level control wrappers.
- `lower-tlam-to-func` handles backend representation change for value-level execution model.

Separating these phases improves:
- diagnosability (clear failure boundary),
- composability (pipeline tuning),
- testability (independent pass-level regressions).

## Recommended usage
Run after `monomorphize,erase-tlam`.

### Minimal safe sequence
`verify -> monomorphize -> erase-tlam -> lower-tlam-to-func -> canonicalize/cse -> verify`

## Relevant tests
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/lower.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/04_erase_lower/lower_chain_two_vlambdas.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/05_pipeline/pipeline.mlir`
- `tests/filecheck/dialects/tlam_de_bruijn/05_pipeline/pass_order.mlir`
- `tests/filecheck/interpreter/full-programs/tlam_dbi_pipeline.mlir`
