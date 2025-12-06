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
        verifyModule(m)
        m
      case _ =>
        op

  // ----------------- top-level walk -----------------

  private def verifyModule(m: ModuleOp): Unit =
    def walkOp(o: Operation): Unit =
      checkOpTypes(o)
      o.regions.foreach(walkRegion)

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.arguments.foreach { arg =>
          checkTypeAttr(arg.typ, None)
        }
        b.operations.foreach(walkOp)
      }

    walkOp(m)

  // ----------------- per-op checks -----------------

  private def checkOpTypes(
      o: Operation
  ): Unit =
    o.results.foreach(r => checkTypeAttr(r.typ, Some(o)))
    o.operands.foreach(v => checkTypeAttr(v.typ, Some(o)))
    o.attributes.values.foreach {
      case d: DepType       => checkDepType(d, Some(o))
      case tv: DlamTVarType => checkDepTypeExpr(tv.expr, Some(o))
      case _                => ()
    }

  private def checkTypeAttr(
      t: Attribute,
      useSite: Option[Operation]
  ): Unit =
    t match
      case d: DepType => checkDepType(d, useSite)
      case _          => ()

  // ----------------- DepType (wrapper attribute) -----------------

  private def checkDepType(
      d: DepType,
      useSite: Option[Operation]
  ): Unit =
    // 1) Collect all Value[Attribute]s referred to inside the dependent type.
    val values = DepTypeAnalysis.collectValues(d.expr)

    // 2) Dominance checks for each such value.
    values.foreach { v =>
      checkValueDominance(v, useSite)
    }

  // ----------------- DepTypeExpr / NatExprExpr recursion -----------------

  private def checkDepTypeExpr(
      e: DepTypeExpr,
      useSite: Option[Operation]
  ): Unit =
    e match
      case TEConst(_) =>
        ()

      case TEFun(in, out) =>
        checkDepTypeExpr(in, useSite)
        checkDepTypeExpr(out, useSite)

      case TEForall(body) =>
        checkDepTypeExpr(body, useSite)

      case TEVec(len, elem) =>
        checkNatExpr(len, useSite)
        checkDepTypeExpr(elem, useSite)

      case TEValueRef(v) =>
        checkValueDominance(v, useSite)

  private def checkNatExpr(
      n: NatExprExpr,
      useSite: Option[Operation]
  ): Unit =
    n match
      case NELit(_) =>
        ()

      case NEAdd(a, b) =>
        checkNatExpr(a, useSite)
        checkNatExpr(b, useSite)

      case NEMul(a, b) =>
        checkNatExpr(a, useSite)
        checkNatExpr(b, useSite)

      case NEFromValue(v) =>
        checkValueDominance(v, useSite)

  // ----------------- Dominance logic -----------------

  /** Dominance check for a value at a given use site.
    *
    *   - If the value is a block argument, it dominates the entire block.
    *   - If the definition and use are in the same block: def must come before
    *     use.
    *   - If they are in different blocks: currently allowed (outer defs can be
    *     used in nested regions).
    */
  private def checkValueDominance(
      v: Value[Attribute],
      useSite: Option[Operation]
  ): Unit =
    useSite.foreach { user =>
      (v.owner, user.container_block) match
        case (Some(block: Block), Some(userBlock)) if block eq userBlock =>
          ()

        case _ =>
          if !isDominated(v, user) then
            throw new Exception(
              s"Dependent type use not dominated by its definition: value=$v, user=${user.name}"
            )
    }

  private def isDominated(
      v: Value[Attribute],
      user: Operation
  ): Boolean =
    val vOwner = v.owner
    val userBlockOpt = user.container_block

    (vOwner, userBlockOpt) match
      case (Some(defBlock: Block), Some(userBlock)) =>
        true

      case (Some(defOp: Operation), Some(userBlock)) =>
        defOp.container_block match
          case Some(defBlock) if defBlock eq userBlock =>
            val ops = userBlock.operations.toSeq
            val defIx = ops.indexOf(defOp)
            val useIx = ops.indexOf(user)
            defIx >= 0 && useIx >= 0 && defIx <= useIx

          case _ =>
            true

      case _ =>
        true
