# *Dependent Types in ScaIR — Architecture & Design Overview*

This document describes how ScaIR implements dependent types that reference SSA values, how such references are resolved, verified, and printed, and how this connects to Dlam operations.

# 1. Motivation

MLIR does *not* allow SSA values to appear in types.

ScaIR extends MLIR by introducing:

* **Dependent type expressions (`DepTypeExpr`)** that *may contain SSA value references* (e.g. `%T`, `%len`).
* **ModuleValueTable**, which gives every SSA value (including *block arguments*) a stable ID.
* **DepTypeSymbolicResolver**, which replaces textual `%T` with `SSAValueId`.
* **DependentTypeVerifierPass**, which verifies dominance and correctness.
* **Printer integration**, so resolved SSA IDs print back using normal `%0`, `%1`, … names.

This enables types like:

```
!dlam.tvar<%T>
!dlam.dep<vec<%len, !dlam.tvar<%T>>>
```


# 2. Value Identification Layer

*(ModuleValueTable & SSAValueId)*

Source: 

Every SSA value in the module (including **block arguments**) is assigned a *stable* identity:

```scala
final case class SSAValueId(defOpId: OpId, resultIndex: Int)
```

Where:

* `defOpId` = *path to the defining op or block header*
* `resultIndex` = result slot (or block argument index)

Example:

```
%T : !dlam.type     // block argument
%len = dlam.nat_source() : i32
```

Might get IDs:

```
%T   → SSAValueId(OpId(List(0,0)), 0)
%len → SSAValueId(OpId(List(0,0,1)), 0)
```

The mapping is provided through:

```scala
val table = ModuleValueTable(module)
table.idOf(value): Option[SSAValueId]
table.valueOf(id): Option[Value[Attribute]]
```

This forms the backbone for dependent typing.

# 3. Dependent Type AST

*(DepTypeExpr & NatExprExpr)*

Source: 

Types may contain *unresolved textual SSA names*:

```scala
TENamedValueRef("T")
NENamedValueRef("len")
```

After resolution they become:

```scala
TEValueRef(SSAValueId(...))
NEFromValue(SSAValueId(...))
```

The full tree includes:

* `TEConst(pureType)`
* `TEFun(in, out)`
* `TEForall(body)`
* `TEVec(lenExpr, elemType)`
* SSA-aware:

  * `TEValueRef(id)`
  * `TENamedValueRef(name)`

# 4. Symbolic Resolution

*(DepTypeSymbolicResolver)*

Source: 

This phase maps textual **`%T` → SSAValueId**:

### Algorithm:

1. Walk the dependent type tree.
2. When encountering:

   * `TENamedValueRef(name)`
     → find SSA value `v` using `v.ssaName == name`
3. Convert it to:

   * `TEValueRef(table.idOf(v))`

### Resolution does **NOT** mutate the IR.

### Example:

```
!dlam.tvar<%T>
```

Parsed as:

```
DlamTVarType(TENamedValueRef("T"))
```

Resolved as:

```
DlamTVarType(TEValueRef(SSAValueId(...)))
```

---

# 5. Dominance Verification

*(DependentTypeVerifierPass)*

Source: 

This pass ensures dependent types never reference *future* (non-dominating) SSA values.

### Steps:

#### 1. Resolve all symbolic refs

```scala
val resolvedExpr = DepTypeSymbolicResolver.resolveDepTypeExpr(...)
```

#### 2. Collect all SSAValueIds

(using `DepTypeAnalysis.collectValueIds`)

#### 3. For each value ID:

* Look up the value in the ModuleValueTable.
* Check dominance against the operation that uses the dependent type.

Block arguments *automatically dominate* their block.

### Example of rejection:

```
%y = ... : !dlam.dep<%x>   // uses %x
%x = ...
```

Fails because `%x` is defined after its use.

# 6. Printer Integration

*(ValueNameResolver & DepTypePrinter)*

Source: Printer.scala + DepTypePrinter in 

The printer must:

1. Ask the ModuleValueTable for the actual SSA value.
2. Ask the Printer to assign the same name (`%0`, `%1`) used everywhere.

### Resolution inside printer:

```scala
case TEValueRef(id) =>
  p.print("%", ValueNameResolver.resolve(id, p))
```

`resolve`:

* fetches the actual `Value` from the table
* calls `p.assignValueName(value)`
* strips the leading `%`

This ensures types use the canonical SSA names.

### Example:

```
!dlam.tvar<TEValueRef(SSAValueId(...))>
```

Prints as:

```
!dlam.tvar<%1>
```

Matching the rest of the IR.

# 7. TLambda, TApply, VLambda — semantics

Source: Ops.scala ()

### `TLambda`

```
"dlam.tlambda"() ({
  ^bb0(%T : !dlam.type):
     ...
}) : () -> !dlam.forall<...>
```

* Introduces **a type parameter** as a *block argument* `%T : !dlam.type`
* Body may reference `%T` via `!dlam.tvar<%T>`

### `TApply`

Instantiates a polymorphic function:

```
%h = dlam.tapply %G, %T
```

Internally uses DBI.instantiate.

### `VLambda`

Value-level lambda:

* Has one block argument
* Function type may depend on SSA values.

# 8. How Everything Works Together

```
               ┌──────────────────────┐
               │  Parsed IR (textual) │
               └──────────┬───────────┘
                          ▼
        ┌─────────────────────────────────────────┐
        │ DepTypeExpr contains TENamedValueRef     │
        └─────────────────────────────────────────┘
                          ▼
     ┌──────────────────────────────────────────────┐
     │ DepTypeSymbolicResolver                       │
     │  - finds SSA values in ModuleValueTable       │
     │  - rewrites %T → TEValueRef(id)               │
     └──────────────────────────────────────────────┘
                          ▼
  ┌────────────────────────────────────────────────────────────────┐
  │ DependentTypeVerifierPass                                      │
  │  - checks all references dominate the use site                 │
  │  - errors if unresolved or illegal SSA reference               │
  └────────────────────────────────────────────────────────────────┘
                          ▼
         ┌──────────────────────────────────┐
         │ Printer + ValueNameResolver      │
         │  - assignValueName               │
         │  - types print using %0, %1      │
         └──────────────────────────────────┘
```

### This yields:

#### Input IR:

```
!dlam.tvar<%T>
```

#### Output IR:

```
!dlam.tvar<%1>   // %1 is the block argument of tlambda
```

# 9. Summary of Guarantees Provided

| Layer                         | Responsibility                                              |
| ----------------------------- | ----------------------------------------------------------- |
| **Parser**                    | Builds unresolved DepTypeExpr with TENamedValueRef          |
| **ModuleValueTable**          | Gives every SSA value (ops + block args) a stable ID        |
| **DepTypeSymbolicResolver**   | Rewrites `%T` → `TEValueRef(SSAValueId(...))`               |
| **DependentTypeVerifierPass** | Ensures dominance + no illegal references                   |
| **Printer**                   | Prints SSA references inside types consistently with the IR |


# 10. Future Extensions

* Allow multiple type parameters: `%T %U %V`
* Add binder variable shadowing
* Support `tapply` as an SSA-level dependent application in types
