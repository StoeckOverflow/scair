---
title: "dTensor Dependent Type Architecture"
---

# dTensor Dependent Type Architecture

Implementation:
- `dialects/src/dtensor/dTensorTypes.scala`
- `dialects/src/dtensor/dTensorOps.scala`
- `dialects/src/dtensor/dTensorUtil.scala`
- `dialects/src/dtensor/dTensorDialect.scala`

Related core infrastructure:
- `core/src/ir/Attribute.scala`
- `core/src/ir/Value.scala`
- `core/src/ir/BlockOperations.scala`
- `core/src/transformations/PatternRewriter.scala`
- `core/src/verify/SSADominanceCheck.scala`

This document describes the current value-dependent tensor architecture used by
the `dTensor` dialect. It is about `!dtensor.*` types such as
`!dtensor.tensor<[%m, %n], f32>`, not ordinary builtin `tensor<...>` types.

## What this dialect does

`dTensor` represents tensor dimensions as SSA-backed natural-number values and
stores references to those values inside tensor type attributes.

For example:

```mlir
%m = "dtensor.nat.param"() : () -> !dtensor.nat
%n = "dtensor.nat.param"() : () -> !dtensor.nat
%t = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
```

The values `%m` and `%n` are not operands of `dtensor.empty`. They are embedded
in the result type of `%t`. This lets the type say that the tensor is shaped by
these exact SSA witnesses, rather than by anonymous dynamic dimensions.

The dialect provides:
- nat-like shape types: `!dtensor.nat`, `!dtensor.posnat`
- dependent tensor types: `!dtensor.vector`, `!dtensor.matrix`,
  `!dtensor.tensor`
- nat-producing operations such as `dtensor.nat.const`, `dtensor.nat.add`, and
  `dtensor.nat.mul`
- bridge operations between nat witnesses and executable index values, such as
  `dtensor.shape.to_index`
- tensor operations whose verification consumes dependent shape provenance,
  such as `dtensor.add`, `dtensor.matmul`, `dtensor.cast`, and
  `dtensor.expand_shape`

## Why this dialect exists

Ordinary dynamic tensor types can describe that a dimension is dynamic, but they
do not identify the SSA value that represents that dimension. If several
different tensor values are semantically same-shaped, ordinary CSE sees only
the operations and operands that recover those dimensions.

`dTensor` makes selected shape facts explicit:
- two tensors can share the same `%m/%n` dimension witnesses;
- a loop bound can be tied to an explicit `dtensor.nat.mul` product;
- `dtensor.dim` can recover an already-known type-carried witness;
- later passes can consume those witnesses before proof erasure or lowering.

The goal is not to provide a general symbolic-shape solver. The goal is to keep
important shape and product facts available as explicit SSA provenance.

## Type and operation structure

`dTensorTypes.scala` defines:

```scala
type DimParam = ValueAttribute
```

This is the central representation rule. A dependent dimension parameter is a
`ValueAttribute`, which points to a ScaIR SSA value.

The shape-carrying types store dimensions as embedded value refs:

```scala
final case class dTensorVectorType(param: DimParam, elem: TypeAttribute)

final case class dTensorMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
)

final case class dTensorTensorType(
    params: Seq[ValueAttribute],
    elem: TypeAttribute,
)
```

`dTensorOps.scala` defines the operations that construct and consume this
provenance:
- `NatParam`, `NatConst`, `NatAdd`, `NatMul`
- `ShapeToIndex`, `IndexToNat`, `NatRefinePositive`
- `Empty`, `Fill`, `Dim`
- `Add`, `Mul`, `Matmul`, `Cast`, `ExpandShape`

`dTensorUtil.scala` contains the shared semantic helpers used by both type and
operation verification:
- `resolveNatValue`
- `resolveNatFromIndexValue`
- `resolveNatProvenance`
- `checkParam`
- `sameDims`
- `orderedNatProductFactors`
- `sameOrderedNatProduct`

## Core representation rule

A valid `dTensor` dimension parameter must resolve to `!dtensor.nat` or
`!dtensor.posnat`.

The helper `dTensorTypeUtil.checkParam` accepts:
1. direct nat-like SSA values;
2. `ValueRefType` wrappers that eventually point to nat-like SSA values.

This second case matters for operations such as `dtensor.dim`, whose result type
is a `!value<...>` reference to the selected embedded dimension.

Invalid dimension parameters are rejected during ordinary type or operation
verification. For example, a dimension reference to an `i32` value is not a
valid dependent tensor dimension.

## Type-use tracking

Dependent dimension refs are not ordinary operand uses. They are tracked in a
separate use set on each SSA value:

```scala
Value.uses      // ordinary operand uses
Value.typeUses  // references embedded in types, attrs, and properties
```

`BlockOperations` registers and unregisters `typeUses` when operations are
inserted, removed, or replaced. It walks:
- result types
- operand types
- operation attributes
- operation properties

This gives ScaIR two use graphs:
1. ordinary dataflow through operands;
2. value-dependent type and metadata references through `typeUses`.

This distinction is intentional. Dimension refs do not clutter operation
operand lists, but they are still visible to rewrites, liveness checks, erasure
guards, and verification.

## Value replacement and type rewrites

Generic RAUW updates dependent tensor refs.

`Rewriter.replaceValue(oldValue, newValue)` first rewrites the old value's
tracked `typeUses`, then rewrites ordinary operand uses. A replacement such as
`%s1 -> %s0` therefore updates:

```mlir
!dtensor.tensor<[%s1], f32>
```

to:

```mlir
!dtensor.tensor<[%s0], f32>
```

This behavior is what lets CSE and canonicalization work on dependent shape
values. If CSE merges two identical `dtensor.nat.add` operations, any tensor
type that referenced the losing result is rewritten to reference the kept
result.

Current limitation: replacement is whole-value replacement. There is no
predicate-scoped `replaceUsesWithIf` equivalent for embedded type refs.

## Liveness and erasure guards

A value used only in a dependent tensor type is still live.

`Value.erase()` rejects erasure when either use set is non-empty:
- `uses`
- `typeUses`

DCE and CSE follow the same rule. An operation result may be erased only if it
has no ordinary uses and no type uses. This prevents dangling refs such as:

```mlir
%m = "dtensor.nat.param"() : () -> !dtensor.nat
%t = "test.make"() : () -> !dtensor.tensor<[%m], f32>
```

where `%m` is semantically required by the type of `%t` even if `%m` has no
ordinary operand users.

## Dominance of embedded refs

`SSADominanceCheck` extends ordinary SSA dominance to value refs embedded in
types, attributes, and properties.

For every operation, the verifier checks:
- ordinary operand dominance;
- embedded value refs in result types;
- embedded value refs in operand types;
- embedded value refs in attributes;
- embedded value refs in properties.

Each embedded `ValueAttribute` is treated as a use at the owning operation. The
referenced value must dominate that operation.

This rejects IR where a dimension is only available on one control-flow path but
is used in a type at a join block:

```mlir
"test.region"() ({
^bb0:
  %c = "arith.constant"() <{value = true}> : () -> i1
  "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
^bb1:
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  "test.br"() [^bb2] : () -> ()
^bb2:
  %t = "test.use"() : () -> !dtensor.tensor<[%m], f32>
  "test.ret"() : () -> ()
}) : () -> ()
```

The operation `test.use` has no ordinary operand use of `%m`, but its result
type depends on `%m`; the verifier rejects the non-dominating embedded use.

## Hierarchical dominance

Uses inside nested regions are checked with hierarchical dominance.

If an embedded type ref appears inside a nested region, the verifier lifts the
use to the ancestor operation located in the definition's region. The outer
definition must dominate that region-owning operation.

This accepts the normal case where an outer dimension witness dominates a
nested computation, and rejects cases where the witness is defined after the
region-owning operation or only on a non-dominating path.

Function examples normally express dynamic dimensions as entry block arguments:

```mlir
func.func @ew_add(%m: !dtensor.nat, %n: !dtensor.nat) {
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>)
      -> !dtensor.tensor<[%m, %n], f32>
  func.return %c : !dtensor.tensor<[%m, %n], f32>
}
```

## Shape equality rule

Basic `dTensor` shape equality is SSA identity after nat resolution.

The helper:

```scala
sameDims(lhs.params, rhs.params)
```

returns true only when corresponding dimensions resolve to the same SSA value.
Two separate constants with the same literal are not considered the same
dimension unless a rewrite such as CSE has made them the same SSA value.

This rule is intentionally stricter than symbolic algebra. It gives local,
cheap verification for:
- `dtensor.add`
- `dtensor.mul`
- `dtensor.cast`
- `dtensor.matmul`

`dtensor.expand_shape` is the main operation with a product-aware rule. It uses
`sameOrderedNatProduct` to check that a source dimension equals the ordered
product of a group of result dimensions. Product matching follows explicit
`dtensor.nat.mul` chains and exact constants. It is not a general Presburger or
symbolic-shape solver.

## Verifier responsibilities

Local `dTensor` verification handles dialect-specific facts:
- dimension params resolve to nat-like values;
- tensor element types are supported scalar element types;
- `dtensor.nat.const` is non-negative, and positive constants are strictly
  positive;
- positive nat operations justify their result type from their operands;
- `dtensor.dim` returns a `!value<...>` pointing at the selected embedded
  dimension;
- elementwise ops require equal element type and pairwise SSA-identical shape;
- `dtensor.matmul` checks rank, element type, inner-dimension identity, and
  result outer dimensions;
- `dtensor.cast` preserves rank, element type, and exact dependent dimensions;
- `dtensor.expand_shape` checks reassociation structure and explicit ordered
  nat-product equality.

Global ScaIR verification handles SSA well-formedness:
- ordinary operand dominance;
- embedded type/attribute/property reference dominance.

There is currently no separate `-verify-dependent-tensor-semantics` pass.
Dependent tensor semantics are enforced by ordinary op/type verification plus
the default `SSADominanceCheck`.

## Behavior on non-ideal input

- A dimension ref that does not resolve to `!dtensor.nat` or
  `!dtensor.posnat` is rejected.
- A non-dominating embedded dimension ref is rejected by
  `SSADominanceCheck`.
- `dtensor.add`, `dtensor.mul`, and `dtensor.cast` reject mismatched shape
  identity even when dimensions may be arithmetically equal.
- `dtensor.matmul` rejects mismatched inner dimensions and result dimensions
  not matching the input outer dimensions.
- `dtensor.expand_shape` rejects reassociation groups whose explicit ordered
  product does not match the source dimension.
- DCE and CSE must not erase values that are live only through `typeUses`.

## Pipeline role

`dTensor` shape and product facts are intended to be consumed before proof
erasure or low-level lowering.

Representative consumers include:
- `tensor-shape-canonicalize`, which folds simple nat expressions and relies on
  deep RAUW to update embedded dims;
- `dependent-dim-query-elim`, which rewrites `dtensor.dim` to type-carried
  provenance;
- exact tiling passes that consume `dtensor.nat.mul` product facts;
- `dependent-tail-min-simplify`, which removes generated tail/min guards when a
  matching nat-product proof is still available;
- `canonicalize-dtensor-nat-products`, which normalizes explicit product facts;
- `erase-dtensor-nat-proofs-to-index`, which erases nat proof structure after
  proof-consuming passes no longer need it.

Typical proof-consuming ordering is:

```text
canonicalize,
cse,
dce,
canonicalize-dtensor-nat-products,
dependent-exact-tile or dependent-tail-min-simplify,
canonicalize,
cse,
dce,
erase-dtensor-nat-proofs-to-index
```

Static affine-compatible routes insert `d-affine-to-affine-compatible` before
proof erasure and before upstream affine normalization/unrolling.

## Known gaps

The current architecture is intentionally lightweight, but a few boundaries are
not yet fully generalized:

- `IsolatedFromAbove` currently verifies ordinary operands, but not embedded
  `typeUses`. Function examples follow the entry-block-argument discipline, and
  global dominance catches many invalid refs, but isolation itself is not yet
  type-use-aware.
- Block argument types are verified for local type well-formedness, but
  embedded refs in block argument types are not registered through
  `BlockOperations` and are not walked by `SSADominanceCheck` in the same way
  operation-owned result and operand types are.
- Generic replacement is whole-value replacement. Predicate-scoped replacement
  of embedded type refs is not yet defined.
- Shape reasoning is intentionally explicit and limited. `sameDims` is SSA
  identity, and product reasoning requires represented `dtensor.nat.mul`
  provenance.

These gaps do not invalidate the current tested `dTensor` path, where dependent
shape refs are primarily carried by operation result and operand types. They are
important for future designs involving persistent loop-carried dependent tensor
refinements, stronger function-boundary contracts, or more MLIR-like scoped
replacement APIs.

