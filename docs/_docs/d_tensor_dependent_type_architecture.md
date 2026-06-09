---
title: "DTensor Dependent Type Architecture"
---

# DTensor Dependent Type Architecture

Implementation:
- `dialects/src/d_tensor/DTensorTypes.scala`
- `dialects/src/d_tensor/DTensorOps.scala`
- `dialects/src/d_tensor/DTensorUtil.scala`
- `dialects/src/d_tensor/DTensorDialect.scala`

Related infrastructure:
- `core/src/ir/Attribute.scala`
- `core/src/ir/Value.scala`
- `core/src/ir/BlockOperations.scala`
- `core/src/transformations/PatternRewriter.scala`
- `core/src/verify/SSADominanceCheck.scala`

`DTensor` is ScaIR's value-dependent tensor surface. It is about types such as
`!d_tensor.tensor<[%m, %n], f32>`, not ordinary builtin `tensor<...>` types.

## Final Shape Model

Dynamic DTensor dimensions are SSA values of builtin `index`. Static integer
dimensions remain first-class. A tensor type can therefore mix both forms:

```mlir
func.func @example(%m: index, %n: index) {
  %mn = "arith.muli"(%m, %n) : (index, index) -> index
  %t = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, 4, %mn], f32>
}
```

The values `%m`, `%n`, and `%mn` are not operands of `d_tensor.empty`. They are
embedded in the result type through the existing `ValueAttribute` machinery. The
embedded reference means the type is shaped by those exact SSA values, rather
than by anonymous dynamic dimensions.

The final surface deliberately follows MLIR conventions:
- dynamic extents use builtin `index`;
- shape arithmetic uses standard `arith.constant`, `arith.addi`, and
  `arith.muli`;
- DTensor vector, matrix, and tensor types embed SSA `index` values directly;
- static dimensions are integer attributes;
- the optional `d_tensor.assume_extent %n : index` operation records
  verification-only extent metadata and does not cast, refine, or produce a new
  value.

Typed size witnesses are future work for a separate branch. The final thesis
surface does not expose a DTensor-specific typed-size or bridge-cast language.

## Why This Exists

Ordinary dynamic tensor types can say that a dimension is dynamic, but they do
not identify the SSA value that carries that dimension. DTensor keeps selected
shape facts explicit:
- two tensors can share the same `%m/%n` dimension values;
- a product dimension can be represented by a shape-rooted `arith.muli` result;
- `d_tensor.dim` can recover the type-carried dimension reference;
- verifier and transformation passes can reason about embedded value identity.

The goal is not to provide a general symbolic shape solver. The goal is to keep
important shape identity and product facts available as ordinary SSA provenance.

## Type Structure

`DTensorTypes.scala` represents dimensions as either static integer attributes
or embedded SSA value references:

```scala
type DimParam = ValueAttribute | IntegerAttr
```

The shape-carrying types store dimensions directly:

```scala
final case class DTensorVectorType(param: DimParam, elem: TypeAttribute)

final case class DTensorMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
)

final case class DTensorTensorType(
    params: Seq[DimParam],
    elem: TypeAttribute,
)
```

A valid dynamic dimension reference must resolve to an SSA value of builtin
`index`. Non-index SSA values, cyclic embedded references, invalid values, and
out-of-scope or non-dominating references are rejected by verification.

## Type-Use Tracking

Embedded dimension refs are not ordinary operands. They are tracked in a
separate use set on each SSA value:

```scala
Value.uses      // ordinary operand uses
Value.typeUses  // references embedded in types, attrs, and properties
```

`BlockOperations` registers and unregisters `typeUses` when operations are
inserted, removed, or replaced. It walks result types, operand types, operation
attributes, and operation properties. This gives ScaIR two use graphs: ordinary
dataflow through operands, and value-dependent metadata references through
`typeUses`.

## Rewrites, RAUW, And Erasure

Generic RAUW updates embedded shape refs. If `%m1` is replaced by `%m0`, a type
like this:

```mlir
!d_tensor.tensor<[%m1], f32>
```

is rewritten to:

```mlir
!d_tensor.tensor<[%m0], f32>
```

A value used only in a dependent tensor type is still live. `Value.erase()` and
DCE reject erasure while either ordinary uses or type uses remain, which prevents
dangling embedded references.

## Dominance And Isolation

`SSADominanceCheck` extends ordinary SSA dominance to value refs embedded in
result types, operand types, attributes, and properties. Each embedded
`ValueAttribute` is treated as a use at the owning operation. The referenced
value must dominate that operation and must respect region isolation rules.

This is what makes direct `%m/%n : index` dimensions behave like normal MLIR SSA
uses even though the use is stored in a type.

## Shape Equality

Shape equality compares static integers by value and dynamic dimensions by the
resolved SSA identity. Two syntactically distinct `ValueAttribute` wrappers are
equal for shape purposes when they resolve to the same SSA value. Two arithmetic
expressions are not algebraically simplified unless a pass has actually
canonicalized them to the same SSA value.

## Product Provenance

Product facts are recovered from shape-rooted `arith.muli : index` operations.
A product is shape-rooted when it is reachable from a DTensor or DMemref shape
position, a DTensor or D-affine shape consumer, or an explicit
`d_tensor.assume_extent` marker that a consumer chooses to honor before it is
erased.

`ShapeIndexProvenance`, `ShapeProductFacts`, and `ShapeDivisibility` recover only
local structural facts:
- constants from `arith.constant : index`;
- additions from `arith.addi : index` where existing consumers need them;
- products from nested `arith.muli : index` rooted in shape use.

They do not globally classify arbitrary index arithmetic as shape provenance and
they do not infer facts through subtraction, division, or remainder.

`d_tensor.assume_extent` is optional verification metadata. It may be erased by
canonicalization or DCE, so correctness cannot depend on it surviving arbitrary
cleanup. It can be used only by passes that explicitly opt into assumption
metadata, and it never represents strict positivity.

## Lowering Role

DTensor lowering preserves direct `index` values. No bridge erasure pass is
needed. Shape-aware optimizations consume the same SSA values that the final IR
uses for loop bounds, strides, and materialized tensor dimensions.
