package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.ModulePass
import scair.dialects.builtin.*
import scair.dialects.tlam_de_bruijn.*

/** Stage-boundary assertion pass:
  * after erase-tlam, no type-level TLam control should remain.
  */
final class CheckPostEraseTLamPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "check-post-erase-tlam"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        checkModule(m)
        m
      case other => other

  private def containsForbiddenType(t: TypeAttribute): Boolean = t match
    case _: tlamForAllType => true
    case _: tlamBVarType   => true
    case tlamFunType(in, out) =>
      containsForbiddenType(in) || containsForbiddenType(out)
    case _ => false

  private def checkTypeAttr(t: TypeAttribute, where: String): Unit =
    if containsForbiddenType(t) then
      sys.error(
        s"check-post-erase-tlam: found post-erase forbidden type at $where: $t"
      )

  private def checkModule(m: ModuleOp): Unit =
    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.arguments.zipWithIndex.foreach { case (a, i) =>
          a.typ match
            case t: TypeAttribute => checkTypeAttr(t, s"block-arg#$i")
            case _                => ()
        }

        b.operations.foreach { op =>
          op match
            case _: TLambda | _: TApply | _: TReturn =>
              sys.error(
                s"check-post-erase-tlam: found type-level op '${op.name}' after erase"
              )
            case _ => ()

          op.operands.zipWithIndex.foreach { case (v, i) =>
            v.typ match
              case t: TypeAttribute => checkTypeAttr(t, s"operand#$i of ${op.name}")
              case _                => ()
          }

          op.results.zipWithIndex.foreach { case (res, i) =>
            res.typ match
              case t: TypeAttribute => checkTypeAttr(t, s"result#$i of ${op.name}")
              case _                => ()
          }

          op.regions.foreach(walkRegion)
        }
      }

    m.regions.foreach(walkRegion)
