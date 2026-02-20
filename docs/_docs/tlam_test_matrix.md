# TLam DB/SSA Test Matrix

This matrix unifies **structure** and **intent** for both TLam variants:
- De Bruijn-indexed type binding (DB)
- SSA-value type binding (SSA-in-types)

Current status: both variants are tested under `tests/filecheck/dialects/tlam`, with DB and SSA cases split by file names and sections.

## Test IDs

Use these IDs in test headers/comments to keep parity explicit.

| ID | Stage | Intent |
|---|---|---|
| WF-01 | verify | `vlambda` region protocol (1 block, 1 arg, `vreturn` last) |
| WF-02 | verify | `tlambda` region protocol (SSA variant: 1 block, 1 type arg, `treturn` last) |
| WF-03 | verify | `vapply` typing (callee fun type, arg/result match) |
| WF-04 | verify | `tapply` typing (forall operand, instantiated result type) |
| WF-05 | verify | DB index bounds/scoping (`bvar` depth discipline) |
| WF-06 | verify | SSA-in-types dominance/use-in-types constraints |
| RW-01 | rewrites | Value beta-reduction succeeds on memory-effect-free body |
| RW-02 | rewrites | Value beta-reduction no-op on effectful/non-direct callee cases |
| RW-03 | rewrites | Capture-avoid and deep RAUW correctness in nested type attrs |
| MONO-01 | monomorphize | `tapply` specialization correctness and elimination at use-sites |
| MONO-02 | monomorphize | DBI/tyArg capture-avoid regression coverage |
| CSE-01 | cse | CSE does not violate type-use isolation/dominance |
| DCE-01 | canon+dce | Dead type-use cleanup remains verifier-safe |
| LOW-01 | erase/lower | No TLam leftovers after erase/lower/reconcile |
| LOW-02 | erase/lower | Negative lowering/reconcile diagnostics stay stable |
| PIPE-01 | pipeline | Full pipeline smoke (valid and invalid cases) |
| PIPE-02 | pipeline | Pipeline idempotence/stability |

## Current Mapping (Existing Files)

| ID | DB coverage | SSA coverage |
|---|---|---|
| WF-01 | `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_types_wf.mlir` (shared structural checks) | `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_types_wf.mlir` |
| WF-02 | `tests/filecheck/dialects/tlam/old_tests/debruijn.mlir` + DB sections in `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_db.mlir` | `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_types_wf.mlir` |
| WF-03 | `tests/filecheck/dialects/tlam/old_tests/debruijn.mlir` | `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_types_wf.mlir` |
| WF-04 | `tests/filecheck/dialects/tlam/old_tests/debruijn.mlir` | `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_types_wf.mlir` |
| WF-05 | `tests/filecheck/dialects/tlam/02_verify/tlam_debruijn_bounds.mlir` | N/A (SSA variant uses WF-06 instead) |
| WF-06 | N/A | `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_dominance_in_types.mlir`, `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_dominance_nested_regions.mlir`, `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_type_uses_in_attrs_and_props.mlir` |
| RW-01 | `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_db.mlir` | `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_ssa.mlir` |
| RW-02 | `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_db.mlir` | `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_ssa.mlir` |
| RW-03 | DB shadowing/capture in `tests/filecheck/dialects/tlam/03_rewrites/tlam_beta_reduce_db.mlir` | `tests/filecheck/dialects/tlam/03_rewrites/tlam_ssa_deep_rauw_regressions.mlir`, `tests/filecheck/dialects/tlam/02_verify/tlam_ssa_tvar_multilocation_nested_params.mlir` |
| MONO-01 | DB-focused sections in `tests/filecheck/dialects/tlam/06_monomorphize/tlam_monomorphize_ssa_and_dbi.mlir` | SSA sections in `tests/filecheck/dialects/tlam/06_monomorphize/tlam_monomorphize_ssa_and_dbi.mlir` |
| MONO-02 | `tests/filecheck/dialects/tlam/06_monomorphize/tlam_monomorphize_ssa_and_dbi.mlir` (DBI sections) | `tests/filecheck/dialects/tlam/06_monomorphize/tlam_ssa_monomorphize_tvar_in_tyarg.mlir` |
| CSE-01 | `tests/filecheck/dialects/tlam/04_cse/tlam_ssa_cse_regressions.mlir` (shared safety expectations) | `tests/filecheck/dialects/tlam/04_cse/tlam_ssa_cse_regressions.mlir`, `tests/filecheck/dialects/tlam/04_cse/tlam_ssa_cse_isolation_regression.mlir` |
| DCE-01 | `tests/filecheck/dialects/tlam/05_canon_dce/tlam_ssa_canon_dce_type_uses.mlir` (shared) | `tests/filecheck/dialects/tlam/05_canon_dce/tlam_ssa_canon_dce_type_uses.mlir` |
| LOW-01 | `tests/filecheck/dialects/tlam/07_lowering/tlam_no_leftovers_after_erase_lower_reconcile.mlir` | `tests/filecheck/dialects/tlam/07_lowering/tlam_no_leftovers_after_erase_lower_reconcile.mlir` |
| LOW-02 | `tests/filecheck/dialects/tlam/07_lowering/tlam_ssa_reconcile_unrealized_cast_negative.mlir` (shared pipeline negative) | `tests/filecheck/dialects/tlam/07_lowering/tlam_ssa_reconcile_unrealized_cast_negative.mlir` |
| PIPE-01 | `tests/filecheck/dialects/tlam/99_pipeline/tlam_pipeline_smoke.mlir` | `tests/filecheck/dialects/tlam/99_pipeline/tlam_pipeline_smoke.mlir` |
| PIPE-02 | `tests/filecheck/dialects/tlam/99_pipeline/tlam_pipeline_idempotence.mlir` | `tests/filecheck/dialects/tlam/99_pipeline/tlam_pipeline_idempotence.mlir` |

## Structure Unification Plan

Target layout (same stage folders for both variants):

- `tests/filecheck/dialects/tlam_db/00_verify/...`
- `tests/filecheck/dialects/tlam_db/03_rewrites/...`
- `tests/filecheck/dialects/tlam_db/06_monomorphize/...`
- `tests/filecheck/dialects/tlam_db/07_lowering/...`
- `tests/filecheck/dialects/tlam_db/99_pipeline/...`

- `tests/filecheck/dialects/tlam_ssa/00_verify/...`
- `tests/filecheck/dialects/tlam_ssa/03_rewrites/...`
- `tests/filecheck/dialects/tlam_ssa/06_monomorphize/...`
- `tests/filecheck/dialects/tlam_ssa/07_lowering/...`
- `tests/filecheck/dialects/tlam_ssa/99_pipeline/...`

Each pair of files should share:
- same test IDs in comments (`// IDs: WF-03, MONO-01`)
- same pass stage and RUN style
- same intent and success/failure condition
- variant-specific assertions only where binding model differs

## Minimal Authoring Template

Add this header at the top of each new test file:

```mlir
// IDs: WF-03, RW-02
// Variant: DB   (or SSA)
// Intent: <one sentence>
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s
```

## Review Rule

Before adding/removing tests:
- every changed behavior must map to at least one ID
- every common ID should have both DB and SSA coverage (unless explicitly marked variant-only)
- pipeline failures should be checked at the earliest responsible stage
