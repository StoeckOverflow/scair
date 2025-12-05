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
        DepTypeSymbolicResolver.resolveInModule(m)
        verifyModule(m)
        m
      case _ =>
        op

  private def verifyModule(m: ModuleOp): Unit =
    def walkOp(o: Operation): Unit =
      checkOpTypes(o, m)
      o.regions.foreach(walkRegion)

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.arguments.foreach { arg =>
          checkTypeAttr(arg.typ, m, None)
        }
        b.operations.foreach(walkOp)
      }

    walkOp(m)

  private def checkOpTypes(
      o: Operation,
      m: ModuleOp
  ): Unit =
    o.results.foreach(r => checkTypeAttr(r.typ, m, Some(o)))
    o.operands.foreach(v => checkTypeAttr(v.typ, m, Some(o)))
    o.attributes.values.foreach {
      case d: DepType       => checkDepType(d, m, Some(o))
      case tv: DlamTVarType => checkDepTypeExpr(tv.expr, m, Some(o))
      case _                =>
    }

  private def checkTypeAttr(
      t: Attribute,
      m: ModuleOp,
      useSite: Option[Operation]
  ): Unit =
    t match
      case d: DepType => checkDepType(d, m, useSite)
      case _          => ()

  private def checkDepType(
      d: DepType,
      m: ModuleOp,
      useSite: Option[Operation]
  ): Unit =
    // 1) assume DepTypeSymbolicResolver.resolveInModule(m) already ran,
    // so no TENamedValueRef / NENamedValueRef left.
    val values = DepTypeAnalysis.collectValues(d.expr)

    // 2) dominance checks
    values.foreach { v =>
      useSite.foreach { user =>
        if !isDominated(v, user) then
          throw new Exception(
            s"Dependent type use not dominated by its definition: value=${v.ssaName.getOrElse("<anon>")}, user=${user.name}"
          )
      }
    }

  private def checkDepTypeExpr(
      e: DepTypeExpr,
      m: ModuleOp,
      useSite: Option[Operation]
  ): Unit =
    e match
      case TEConst(_) =>
        ()

      case TEFun(in, out) =>
        checkDepTypeExpr(in, m, useSite)
        checkDepTypeExpr(out, m, useSite)

      case TEForall(body) =>
        checkDepTypeExpr(body, m, useSite)

      case TEVec(len, elem) =>
        checkNatExpr(len, m, useSite)
        checkDepTypeExpr(elem, m, useSite)

      case TEValueRef(v) =>
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
      useSite: Option[Operation]
  ): Unit =
    n match
      case NELit(_) =>
        ()

      case NEAdd(a, b) =>
        checkNatExpr(a, m, useSite)
        checkNatExpr(b, m, useSite)

      case NEMul(a, b) =>
        checkNatExpr(a, m, useSite)
        checkNatExpr(b, m, useSite)

      case NEFromValue(v) =>
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

  /** Dominance check for a value at a given use site.
    *
    * For now we implement:
    *   - If def and use are in the same block: def must come before use.
    *   - If they are in different blocks: allow (outer defs can be used in
    *     nested regions).
    *   - Block arguments dominate their whole block and any nested regions.
    */
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
