package scair.dialects.dlam

import scair.ir.*
import scair.dialects.builtin.ModuleOp

/** Resolve symbolic SSA references in DepTypeExpr / NatExprExpr
  * (TENamedValueRef, NENamedValueRef) to SSAValueId-based nodes (TEValueRef,
  * NEFromValue).
  *
  * This is a *pure* resolver: it does not mutate the IR, it just returns new
  * expression trees.
  */
object DepTypeSymbolicResolver:

  /** Public entry point: resolve a DepTypeExpr using the given module + table.
    */
  def resolveDepTypeExpr(
      expr: DepTypeExpr,
      module: ModuleOp,
      table: ModuleValueTable
  ): DepTypeExpr =
    resolveDepTypeExprRec(expr, module, table)

  /** Public entry point: resolve a NatExprExpr using the given module + table.
    */
  def resolveNatExpr(
      expr: NatExprExpr,
      module: ModuleOp,
      table: ModuleValueTable
  ): NatExprExpr =
    resolveNatExprRec(expr, module, table)

  // ---------------- internal helpers ----------------

  /** Find the Value whose textual SSA name is `name` (e.g. "x" for "%x") by
    * scanning the module.
    *
    * We look at:
    *   - op results (r.ssaName)
    *   - block arguments (arg.ssaName)
    */
  private def findValueByName(
      m: ModuleOp,
      name: String
  ): Option[Value[Attribute]] =
    var found: Option[Value[Attribute]] = None

    def visitOp(op: Operation): Unit =
      if found.isDefined then return

      // results of this op
      op.results.foreach { r =>
        r.ssaName match
          case Some(n) if n == name =>
            found = Some(r)
          case _ => ()
      }

      // nested regions
      op.regions.foreach { r =>
        r.blocks.foreach { b =>
          // block arguments
          b.arguments.foreach { arg =>
            arg.ssaName match
              case Some(n) if n == name =>
                found = Some(arg)
              case _ => ()
          }
          // operations inside block
          b.operations.foreach(visitOp)
        }
      }

    visitOp(m)
    found

  // Recursive resolution for DepTypeExpr
  private def resolveDepTypeExprRec(
      e: DepTypeExpr,
      m: ModuleOp,
      table: ModuleValueTable
  ): DepTypeExpr =
    e match
      case TEConst(pure) =>
        TEConst(pure)

      case TEFun(in, out) =>
        TEFun(
          resolveDepTypeExprRec(in, m, table),
          resolveDepTypeExprRec(out, m, table)
        )

      case TEForall(body) =>
        TEForall(resolveDepTypeExprRec(body, m, table))

      case TEVec(len, elem) =>
        TEVec(
          resolveNatExprRec(len, m, table),
          resolveDepTypeExprRec(elem, m, table)
        )

      // already resolved
      case TEValueRef(id) =>
        TEValueRef(id)

      // symbolic: %name
      case TENamedValueRef(name) =>
        val v =
          findValueByName(m, name).getOrElse {
            throw new Exception(
              s"[DepTypeSymbolicResolver] Could not resolve SSA name %$name in DepType"
            )
          }

        val id =
          table.idOf(v).getOrElse {
            throw new Exception(
              s"[DepTypeSymbolicResolver] ModuleValueTable has no id for value %$name (owner=${v.owner})"
            )
          }

        TEValueRef(id)

  // Recursive resolution for NatExprExpr
  private def resolveNatExprRec(
      n: NatExprExpr,
      m: ModuleOp,
      table: ModuleValueTable
  ): NatExprExpr =
    n match
      case NELit(v) =>
        NELit(v)

      case NEAdd(a, b) =>
        NEAdd(
          resolveNatExprRec(a, m, table),
          resolveNatExprRec(b, m, table)
        )

      case NEMul(a, b) =>
        NEMul(
          resolveNatExprRec(a, m, table),
          resolveNatExprRec(b, m, table)
        )

      // already resolved
      case NEFromValue(id) =>
        NEFromValue(id)

      // symbolic: %name
      case NENamedValueRef(name) =>
        val v =
          findValueByName(m, name).getOrElse {
            throw new Exception(
              s"[DepTypeSymbolicResolver] Could not resolve SSA name %$name in NatExpr"
            )
          }

        val id =
          table.idOf(v).getOrElse {
            throw new Exception(
              s"[DepTypeSymbolicResolver] ModuleValueTable has no id for value %$name (owner=${v.owner})"
            )
          }

        NEFromValue(id)

  def resolveInModule(module: ModuleOp): Unit =
    // Build a fresh value table for this module
    val table = ModuleValueTable(module)

    // Try to resolve any dependent-typed attribute we see.
    // We ignore the result, because the resolver is pure and the IR is immutable.
    def resolveTypeAttr(attr: Attribute): Unit =
      attr match
        // !dlam.dep<...>
        case DepType(expr) =>
          // Will throw if any TENamedValueRef / NENamedValueRef cannot be resolved
          resolveDepTypeExpr(expr, module, table)
          ()
        // !dlam.tvar<...>
        case DlamTVarType(expr) =>
          resolveDepTypeExpr(expr, module, table)
          ()
        case _ =>
          ()

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        // Block argument types
        b.arguments.foreach { arg =>
          resolveTypeAttr(arg.typ)
        }
        // Operations in the block
        b.operations.foreach(walkOp)
      }

    def walkOp(o: Operation): Unit =
      // Result types
      o.results.foreach { res =>
        resolveTypeAttr(res.typ)
      }

      // Operand types
      o.operands.foreach { v =>
        resolveTypeAttr(v.typ)
      }

      // Attributes on the op
      o.attributes.values.foreach(resolveTypeAttr)

      // Nested regions
      o.regions.foreach(walkRegion)

    // Kick off from the top-level module op
    walkOp(module)
