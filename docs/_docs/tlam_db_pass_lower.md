# Pass: `lower-tlam-to-func`

File: `dialects/src/tlam_de_bruijn/LowerTlamToFuncPass.scala`

## Goal
Lower remaining value-level TLam constructs into `func` dialect:
- `vlambda` -> lifted `func.func` + `func.constant` symbol value
- `vapply`  -> `func.call_indirect`
- `vreturn` -> `func.return`

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
- Uses are replaced by rebuilding user ops with updated operands (not by mutating operand lists in place).
- This keeps parent links and use-def chains valid in this IR framework.

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

## Current limitations
- Expects lowered `VApply` callee values to already have builtin `FunctionType`.
- Throws exception if unexpected callee type remains at rewrite time.
- Type-level TLam ops should be erased beforehand (`erase-tlam`).

### Practical implications
- If `erase-tlam` has not run (or did not fully normalize type-level constructs), lowering may fail.
- If some callee path still carries TLam type instead of builtin `FunctionType`, lowering throws by design.
- The pass intentionally prefers explicit failure over silent mis-lowering.

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
