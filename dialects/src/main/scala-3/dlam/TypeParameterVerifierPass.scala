package scair.passes

import scair.MLContext
import scair.ir.*
import scair.dialects.builtin.*
import scair.transformations.*
import scair.dialects.dlam.*

class TypeParameterVerifierPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "verify-type-params"

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
        b.arguments.foreach(arg => checkTypeAttr(arg.typ, None))
        b.operations.foreach(walkOp)
      }

    walkOp(m)

  // ----------------- per-op checks -----------------

  private def checkOpTypes(o: Operation): Unit =
    o.results.foreach(r => checkTypeAttr(r.typ, Some(o)))
    o.operands.foreach(v => checkTypeAttr(v.typ, Some(o)))
    o.attributes.values.foreach {
      case tv: DlamTVarType => checkTVarType(tv, Some(o))
      case _                => ()
    }

  private def checkTypeAttr(t: Attribute, useSite: Option[Operation]): Unit =
    t match
      case tv: DlamTVarType => checkTVarType(tv, useSite)
      case _                => ()

  private def checkTVarType(
      tv: DlamTVarType,
      useSite: Option[Operation],
  ): Unit =
    checkValueDominance(tv.tparam, useSite)

  // ----------------- Dominance logic -----------------

  private def checkValueDominance(
      v: Value[Attribute],
      useSite: Option[Operation],
  ): Unit =
    useSite.foreach { user =>
      if !isDominated(v, user) then
        throw new Exception(
          s"Type parameter not dominated by its definition: tparam=$v, user=${user
              .name}"
        )
    }

  private def isDominated(v: Value[Attribute], user: Operation): Boolean =
    val vOwner = v.owner
    val userBlockOpt = user.containerBlock

    (vOwner, userBlockOpt) match
      case (Some(_: Block), Some(_)) =>
        true

      case (Some(defOp: Operation), Some(userBlock)) =>
        defOp.containerBlock match
          case Some(defBlock) if defBlock eq userBlock =>
            val ops = userBlock.operations.toSeq
            val defIx = ops.indexOf(defOp)
            val useIx = ops.indexOf(user)
            defIx >= 0 && useIx >= 0 && defIx <= useIx
          case _ =>
            true

      case _ =>
        true
