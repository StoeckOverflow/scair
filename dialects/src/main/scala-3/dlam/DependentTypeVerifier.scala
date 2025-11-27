package scair.passes

import scair.MLContext
import scair.ir.*
import scair.dialects.builtin.*
import scair.transformations.*
import scair.dialects.dlam.*

class DependentTypeVerifierPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "verify-dependent-types"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        val table = ModuleValueTable(m)
        verifyModule(m, table)
        m
      case _ =>
        op

  private def verifyModule(m: ModuleOp, table: ModuleValueTable): Unit =
    def walkOp(o: Operation): Unit =
      checkOpTypes(o, m, table)
      o.regions.foreach(walkRegion)

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        // block args types
        b.arguments.foreach { arg =>
          checkTypeAttr(arg.typ, m, table, None)
        }
        b.operations.foreach(walkOp)
      }

    walkOp(m)

  private def checkOpTypes(
      o: Operation,
      m: ModuleOp,
      table: ModuleValueTable
  ): Unit =
    // result types
    o.results.foreach(r => checkTypeAttr(r.typ, m, table, Some(o)))
    // operand types
    o.operands.foreach(v => checkTypeAttr(v.typ, m, table, Some(o)))
    // attributes
    o.attributes.values.foreach {
      case d: DepType       => checkDepType(d, m, table, Some(o))
      case tv: DlamTVarType => checkDepTypeExpr(tv.expr, m, table, Some(o))
      case _                =>
    }

  private def checkTypeAttr(
      t: Attribute,
      m: ModuleOp,
      table: ModuleValueTable,
      useSite: Option[Operation]
  ): Unit =
    t match
      case d: DepType => checkDepType(d, m, table, useSite)
      case _          => ()

  private def checkDepType(
      d: DepType,
      m: ModuleOp,
      table: ModuleValueTable,
      useSite: Option[Operation]
  ): Unit =
    // 1) resolve TENamedValueRef / NENamedValueRef -> TEValueRef / NEFromValue
    val resolvedExpr =
      DepTypeSymbolicResolver.resolveDepTypeExpr(d.expr, m, table)

    // 2) extract all SSAValueIds from the resolved tree
    val ids = DepTypeAnalysis.collectValueIds(resolvedExpr)

    // 3) dominance checks as before
    ids.foreach { id =>
      table.valueOf(id) match
        case None =>
          throw new Exception(
            s"Dependent type refers to dead or unknown value: $id"
          )
        case Some(v) =>
          useSite.foreach { user =>
            if !isDominated(v, user) then
              throw new Exception(
                s"Dependent type use not dominated by its definition: valueId=$id, user=${user.name}"
              )
          }
    }

  private def checkDepTypeExpr(
      e: DepTypeExpr,
      m: ModuleOp,
      table: ModuleValueTable,
      useSite: Option[Operation]
  ): Unit =
    e match
      case TEConst(_) =>
        ()

      case TEFun(in, out) =>
        checkDepTypeExpr(in, m, table, useSite)
        checkDepTypeExpr(out, m, table, useSite)

      case TEForall(body) =>
        checkDepTypeExpr(body, m, table, useSite)

      case TEVec(len, elem) =>
        checkNatExpr(len, m, table, useSite)
        checkDepTypeExpr(elem, m, table, useSite)

      case TEValueRef(id) =>
        table.valueOf(id) match
          case None =>
            throw new Exception(
              s"Dependent type refers to dead or unknown SSAValueId: $id"
            )
          case Some(v) =>
            checkValueDominance(v, useSite)

      case TENamedValueRef(name) =>
        findValueByName(m, name) match
          case None =>
            throw new Exception(
              s"Dependent type refers to unknown SSA value %$name"
            )
          case Some(v) =>
            checkValueDominance(v, useSite)

  private def checkNatExpr(
      n: NatExprExpr,
      m: ModuleOp,
      table: ModuleValueTable,
      useSite: Option[Operation]
  ): Unit =
    n match
      case NELit(_) =>
        ()

      case NEAdd(a, b) =>
        checkNatExpr(a, m, table, useSite)
        checkNatExpr(b, m, table, useSite)

      case NEMul(a, b) =>
        checkNatExpr(a, m, table, useSite)
        checkNatExpr(b, m, table, useSite)

      case NEFromValue(id) =>
        table.valueOf(id) match
          case None =>
            throw new Exception(
              s"NatExpr refers to dead or unknown SSAValueId: $id"
            )
          case Some(v) =>
            checkValueDominance(v, useSite)

      case NENamedValueRef(name) =>
        findValueByName(m, name) match
          case None =>
            throw new Exception(
              s"NatExpr refers to unknown SSA value %$name"
            )
          case Some(v) =>
            checkValueDominance(v, useSite)

  private def findValueByName(
      m: ModuleOp,
      name: String
  ): Option[Value[Attribute]] =
    var found: Option[Value[Attribute]] = None

    def visitOp(op: Operation): Unit =
      if found.isDefined then return

      // op results
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

  /** Dominance check for a value at a given use site.
    *
    *   - If the value is an op result, we use the existing isDominated logic.
    *   - If the value is a block argument, we treat it as dominating everything
    *     in its block (standard SSA semantics).
    */
  private def checkValueDominance(
      v: Value[Attribute],
      useSite: Option[Operation]
  ): Unit =
    useSite.foreach { user =>
      (v.owner, user.container_block) match
        case (Some(block: Block), Some(userBlock)) if block eq userBlock =>
          () // block argument dominates within its block

        case _ =>
          if !isDominated(v, user) then
            throw new Exception(
              s"Dependent type use not dominated by its definition: value=$v, user=${user.name}"
            )
    }

  /** Very conservative dominance: only allow uses later in the *same block*. If
    * def and use are in different blocks/regions, treat as illegal for now.
    */
  private def isDominated(
      v: Value[Attribute],
      user: Operation
  ): Boolean =
    (v.owner, user.container_block) match
      case (Some(defOp: Operation), Some(block))
          if defOp.container_block.contains(block) =>
        val ops = block.operations.toSeq
        val defIdx = ops.indexOf(defOp)
        val useIdx = ops.indexOf(user)
        defIdx >= 0 && useIdx >= 0 && defIdx <= useIdx
      case _ =>
        // different block / unknown: disallow for now
        false
