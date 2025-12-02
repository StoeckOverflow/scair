package scair.dialects.dlam

import scair.ir.*
import scair.dialects.builtin.ModuleOp

/** Resolve symbolic SSA references in DepTypeExpr / NatExprExpr
  * (TENamedValueRef, NENamedValueRef) to value-based nodes (TEValueRef,
  * NEFromValue).
  *
  * This is a resolver over the dependent-type expression trees. The pure
  * helpers (resolveDepTypeExpr / resolveNatExpr) return new trees; the
  * resolveInModule entry point mutates DepType / DlamTVarType / DlamFunType /
  * DlamForAllType attributes in-place.
  */
object DepTypeSymbolicResolver:

  /** Public entry point: resolve a DepTypeExpr using the given module. */
  def resolveDepTypeExpr(
      expr: DepTypeExpr,
      module: ModuleOp
  ): DepTypeExpr =
    resolveDepTypeExprRec(expr, module)

  /** Public entry point: resolve a NatExprExpr using the given module. */
  def resolveNatExpr(
      expr: NatExprExpr,
      module: ModuleOp
  ): NatExprExpr =
    resolveNatExprRec(expr, module)

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

  // Recursive resolution for DepTypeExpr → TEValueRef(Value)
  private def resolveDepTypeExprRec(
      e: DepTypeExpr,
      m: ModuleOp
  ): DepTypeExpr =
    e match
      case TEConst(pure) =>
        TEConst(pure)

      case TEFun(in, out) =>
        TEFun(
          resolveDepTypeExprRec(in, m),
          resolveDepTypeExprRec(out, m)
        )

      case TEForall(body) =>
        TEForall(resolveDepTypeExprRec(body, m))

      case TEVec(len, elem) =>
        TEVec(
          resolveNatExprRec(len, m),
          resolveDepTypeExprRec(elem, m)
        )

      // already resolved → keep as-is
      case tev @ TEValueRef(_) =>
        tev

      // symbolic: %name
      case TENamedValueRef(name) =>
        val v =
          findValueByName(m, name).getOrElse {
            throw new Exception(
              s"[DepTypeSymbolicResolver] Could not resolve SSA name %$name in DepType"
            )
          }
        TEValueRef(v)

  // Recursive resolution for NatExprExpr → NEFromValue(Value)
  private def resolveNatExprRec(
      n: NatExprExpr,
      m: ModuleOp
  ): NatExprExpr =
    n match
      case NELit(v) =>
        NELit(v)

      case NEAdd(a, b) =>
        NEAdd(
          resolveNatExprRec(a, m),
          resolveNatExprRec(b, m)
        )

      case NEMul(a, b) =>
        NEMul(
          resolveNatExprRec(a, m),
          resolveNatExprRec(b, m)
        )

      // already resolved
      case nev @ NEFromValue(_) =>
        nev

      // symbolic: %name
      case NENamedValueRef(name) =>
        val v =
          findValueByName(m, name).getOrElse {
            throw new Exception(
              s"[DepTypeSymbolicResolver] Could not resolve SSA name %$name in NatExpr"
            )
          }

        NEFromValue(v)

  /** Resolve all symbolic dependent-type / nat expressions in the module.
    *
    * This *mutates* the IR in-place:
    *
    *   - DepType.expr and DlamTVarType.expr are rewritten so that any
    *     TENamedValueRef / NENamedValueRef become TEValueRef / NEFromValue.
    *   - DlamFunType and DlamForAllType are recursively traversed so that
    *     DlamTVarType occurrences inside them are also rewritten.
    *
    * After this pass, all dependent-type expressions reachable from `module`
    * should be fully SSA-based (no symbolic %names).
    */
  def resolveInModule(module: ModuleOp): Unit =

    // Mutating helper: walk any Attribute that may embed DepTypeExpr/NatExprExpr.
    def resolveTypeAttr(a: Attribute): Unit =
      a match
        // !dlam.dep<...>
        case d: DepType =>
          d.expr = resolveDepTypeExprRec(d.expr, module)

        // !dlam.tvar<...>
        case tv: DlamTVarType =>
          tv.expr = resolveDepTypeExprRec(tv.expr, module)

        // !dlam.fun<in, out> – recurse structurally
        case fun: DlamFunType =>
          resolveTypeAttr(fun.in)
          resolveTypeAttr(fun.out)

        // !dlam.forall<body> – recurse into body type
        case fa: DlamForAllType =>
          resolveTypeAttr(fa.body)

        case _ =>
          ()

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        // block args types
        b.arguments.foreach(arg => resolveTypeAttr(arg.typ))
        // operations
        b.operations.foreach(walkOp)
      }

    def walkOp(o: Operation): Unit =
      // result types
      o.results.foreach(res => resolveTypeAttr(res.typ))

      // operand types
      o.operands.foreach(v => resolveTypeAttr(v.typ))

      // op attributes (funAttr, expected, etc.)
      o.attributes.values.foreach(resolveTypeAttr)
      o.properties.values.foreach(resolveTypeAttr)

      // nested regions
      o.regions.foreach(walkRegion)

    // Kick off from the top-level module op
    walkOp(module)
