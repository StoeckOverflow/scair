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

      op.results.foreach { r =>
        r.ssaName match
          case Some(n) if n == name =>
            found = Some(r)
          case _ => ()
      }

      op.regions.foreach { r =>
        r.blocks.foreach { b =>
          b.arguments.foreach { arg =>
            arg.ssaName match
              case Some(n) if n == name =>
                found = Some(arg)
              case _ => ()
          }
          b.operations.foreach(visitOp)
        }
      }

    visitOp(m)
    found

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

      case TEValueRef(id) =>
        TEValueRef(id)

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

      case NEFromValue(id) =>
        NEFromValue(id)

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

  /** Resolve all symbolic dependent-type / nat expressions in the module.
    *
    * This *mutates* the IR in-place:
    *
    *   - DepType.expr and DlamTVarType.expr are rewritten so that any
    *     TENamedValueRef / NENamedValueRef become TEValueRef / NEFromValue.
    *
    * After this pass, all dependent-type expressions reachable from `module`
    * should be fully SSA-based (no symbolic %names).
    */
  def resolveInModule(module: ModuleOp): Unit =
    val table = ModuleValueTable(module)

    // Mutate a single Attribute in-place if it is a dlam dependent type
    def resolveTypeAttrInPlace(attr: Attribute): Unit =
      attr match
        // !dlam.dep<...>
        case d: DepType =>
          d.expr = resolveDepTypeExpr(d.expr, module, table)

        // !dlam.tvar<...>
        case tv: DlamTVarType =>
          tv.expr = resolveDepTypeExpr(tv.expr, module, table)

        // Any compound attribute (fun, forall, vec, etc.) that exposes children
        case pa: ParametrizedAttribute =>
          pa.parameters.foreach {
            case a: Attribute =>
              resolveTypeAttrInPlace(a)
            case seq: Seq[?] =>
              // parameters is Seq[Attribute | Seq[Attribute]]; recurse into nested sequences
              seq.asInstanceOf[Seq[Attribute]].foreach(resolveTypeAttrInPlace)
          }

        case _ =>
          ()

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.arguments.foreach { arg =>
          resolveTypeAttrInPlace(arg.typ)
        }

        b.operations.foreach(walkOp)
      }

    def walkOp(o: Operation): Unit =
      o.results.foreach { res =>
        resolveTypeAttrInPlace(res.typ)
      }

      o.operands.foreach { v =>
        resolveTypeAttrInPlace(v.typ)
      }

      o.attributes.values.foreach { attr =>
        resolveTypeAttrInPlace(attr)
      }

      o.regions.foreach(walkRegion)

    walkOp(module)
