# DBI Engine for `tlam_de_bruijn`

This document explains the De Bruijn Index (DBI) engine used by the DB-only TLam dialect:
- type-level index shifting,
- capture-avoiding substitution,
- forall instantiation,
- and why these rules are correct.

Implementation source:
- `dialects/src/tlam_de_bruijn/TlamDeBruijnTypes.scala` (`object DBI`)
- `dialects/src/tlam_de_bruijn/verify/DeBruijnIndicesCheck.scala`

## 1. Type language and binder depth
Relevant type constructors:
- `!tlam.bvar<k>`: De Bruijn variable index `k`
- `!tlam.fun<in,out>`
- `!tlam.forall<body>`

Well-formedness criterion at an occurrence site with depth `d`:
- `0 <= k < d`

Depth increases by 1 when entering a type binder (`forall`) and, in operation-level checking, when entering `tlambda` regions.

## 2. DBI primitives in code

### `shift(d, c, t)`
`DBI.shift` increases indices `>= c` by `d`:
- `bvar(k)` with `k >= c` becomes `bvar(k + d)`
- recurses through `fun`
- on `forall(body)`, recurses as `shift(d, c + 1, body)`

Intuition:
- `shift` adjusts free references when moving terms under additional binders.
- `c` is the cutoff: indices below cutoff are locally bound and must not move.

### `subst(c, s, t)`
`DBI.subst` substitutes index `c` with type `s` in `t`:
- if `k == c`: replace with `s`
- if `k > c`: decrement to `k - 1` (binder removal effect)
- if `k < c`: unchanged
- recurses through `fun`
- on `forall(body)`: recurse with
  `subst(c + 1, shift(1, 0, s), body)`

This `shift(1,0,s)` under binder entry is the core capture-avoid step.

### `instantiate(fa, arg)`
`DBI.instantiate(fa, arg)` is implemented as:
- `subst(0, arg, fa.body)`

This is standard `forall` instantiation at binder index 0.

## 3. Why this is capture-avoiding
Key invariant:
- When substitution descends under one binder, the replacement term is shifted by one (`shift(1,0,s)`) before recursive substitution.

Why this matters:
- Without this shift, free indices inside `s` could be accidentally captured by the new binder encountered in `body`.
- With the shift, those free indices are lifted to remain free relative to the deeper environment.

This is the canonical de Bruijn substitution rule.

## 4. Worked examples

## Example A: simple instantiation
`forall(fun(bvar<0>, bvar<0>)) [i32]`

Steps:
1. `instantiate = subst(0, i32, fun(bvar<0>, bvar<0>))`
2. each `bvar<0>` matches `k == c`, replaced by `i32`
3. result: `fun(i32, i32)`

## Example B: substitution under nested forall (capture avoidance)
Suppose we substitute `c=0`, `s=bvar<0>` into `forall(fun(bvar<1>, bvar<0>))`.

Descending into `forall`:
- recursive call uses `c=1`
- replacement becomes `shift(1,0,bvar<0>) = bvar<1>`

Inside body `fun(bvar<1>, bvar<0>)` with `c=1, s=bvar<1>`:
- `bvar<1>` matches -> replaced by `bvar<1>`
- `bvar<0>` is below cutoff -> unchanged

Result remains structurally correct and no free variable is captured.

## Example C: decrement rule (`k > c`)
Substitute `c=0` in `bvar<2>`:
- since `2 > 0`, result is `bvar<1>`.

This reflects that removing binder 0 shifts outer references inward by one.

## 5. Interaction with monomorphization
In `monomorphize`, specialization may occur at nonzero binder depth. The pass uses:
- `instAt(t, tyArg, depth)`
- where `tyArg` is pre-shifted by `depth` before substitution

This is necessary when specializing terms nested under binders so that free references in `tyArg` remain correctly scoped.

Implementation:
- `dialects/src/tlam_de_bruijn/Monomorphize.scala` (`instAt`)

## 6. Runtime verification of DBI well-scoping
`DeBruijnIndicesCheck` validates that every encountered `bvar<k>` satisfies bounds at traversal depth:
- checks type positions in operands/results/block arguments,
- increments depth under type binders (`forall`) and `tlambda` regions.

This is the safety net that catches out-of-scope indices after parsing/transforms.

Implementation:
- `dialects/src/tlam_de_bruijn/verify/DeBruijnIndicesCheck.scala`

## 7. Why this engine is sufficient for DB-only mode
Given:
1. `shift` as defined above,
2. `subst` with binder-entry shift,
3. `instantiate = subst(0, arg, body)`,
4. verifier enforcing `0 <= k < depth`,

the dialect has the standard, textbook DBI machinery required for capture-avoiding type substitution and forall instantiation in a DB-only representation.

## 8. Practical limitations
- This engine reasons at type-attribute level; correctness still depends on passes calling it at the right depth.
- If a transform manually rewrites types without `shift/subst`, verifier can still catch many scoping errors, but semantic intent may be wrong.
- The system is deterministic and rule-based; no solver-based equivalence checking is performed.
