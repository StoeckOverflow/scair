---
title: "TLam DBI Engine (SSA-in-types dialect)"
---

# TLam DBI Engine

This document explains the de Bruijn substitution utilities and DBI verifier used by the current TLam dialect implementation in `dialects/src/tlam`.

Implementation sources:
- `dialects/src/tlam/TlamTypes.scala` (`object DBI`)
- `dialects/src/tlam/verify/DeBruijnIndicesCheck.scala`

## Type constructors involved

The DBI engine works over these type constructors:
- `!tlam.bvar<k>` (`TlamBVarType`)
- `!tlam.fun<in,out>` (`TlamFunType`)
- `!tlam.forall<body>` (`TlamForAllType`)

The dialect also has `!tlam.tvar<%x>` (`TlamTVarType`) for SSA-in-types, but DBI operations only rewrite `bvar` and recurse through container types.

## DBI operations in code

## `shift(d, c, t)`

Defined in `DBI.shift`.

Behavior:
1. If `t` is `bvar(k)` and `k >= c`, rewrite to `bvar(k + d)`.
2. Recurse through function type input/output.
3. Under `forall`, recurse with cutoff `c + 1`.
4. Leave non-DBI constructors unchanged.

Purpose: adjust free de Bruijn indices when crossing binders.

## `subst(c, s, t)`

Defined in `DBI.subst`.

Behavior:
1. If `t` is `bvar(k)` and `k == c`, replace with `s`.
2. If `k > c`, decrement to `bvar(k - 1)`.
3. If `k < c`, keep unchanged.
4. Recurse through `fun`.
5. Under `forall`, recurse as:
   - `subst(c + 1, shift(1, 0, s), body)`

That `shift(1,0,s)` is the capture-avoid rule.

## `instantiate(fa, arg)`

Defined as `subst(0, arg, fa.body)`.

Used by `tlam.tapply` verifier (`Ops.scala`) and monomorphization logic to compute instantiated result type.

## Why this is capture-avoiding

When substitution enters a deeper binder, the replacement type `s` is shifted by one. This prevents free DBI references inside `s` from being captured by the binder just entered.

## Runtime DBI well-formedness check

`DeBruijnIndicesCheck` (`verify/DeBruijnIndicesCheck.scala`) validates that every `bvar<k>` is in scope.

Traversal checks:
1. block argument types
2. operand types
3. result types
4. attributes and properties
5. type arguments in `tlam.tapply`

Depth handling:
- start at depth `0`
- enter `TLambda` body with `depth + 1`
- enter `TlamForAllType(body)` with `depth + 1`

Acceptance rule: `0 <= k < depth`; else emit `debruijn: bvar<k> out of scope at depth=<depth>`.

## Interaction with SSA-in-types (`tvar`)

DBI utilities do not rewrite `tvar` directly. In this dialect, type substitution during monomorphization combines:
1. `DBI.subst` for `bvar`
2. explicit `substTVar` for `!tlam.tvar<%binder>` references

That combined behavior is documented in `tlam_pass_monomorphize.md`.
