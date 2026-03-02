package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.RewriteMethods
import scair.transformations.*
import scair.dialects.builtin.*
import scair.dialects.tlam_de_bruijn.*

final class EraseTLamPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "erase-tlam"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        eraseInModule(m); m
      case other => other

  private def eraseInModule(m: ModuleOp): Unit =
    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        val ops = b.operations.toSeq
        ops.foreach {
          case tl: TLambda =>
            // Erase nested TLambda ops first, then erase this one.
            walkRegion(tl.body)
            if tl.res.uses.isEmpty then
              tl.body.blocks.toSeq match
                case Seq(bodyBlock) =>
                  val bodyOps = bodyBlock.operations.toSeq
                  bodyOps.lastOption match
                    case Some(tret: TReturn) =>
                      val moved = bodyOps.dropRight(1).map(bodyBlock.detachOp)
                      RewriteMethods.insertOpsBefore(tl, moved)
                      RewriteMethods.replaceOp(
                        tl,
                        newOps = Seq.empty,
                        newResults = Some(Seq(tret.value)),
                      )
                    case None if bodyOps.isEmpty =>
                      RewriteMethods.eraseOp(tl)
                    case _ =>
                      // Malformed TLambda with payload ops: leave unchanged and
                      // let verifier report under --verify-diagnostics.
                      ()
                case blocks if blocks.forall(_.operations.isEmpty) =>
                  RewriteMethods.eraseOp(tl)
                case _ =>
                  // Multi-block or otherwise malformed TLambda with payload
                  // ops: leave unchanged and let verifier report.
                  ()
            else ()

          case other =>
            other.regions.foreach(walkRegion)
        }
      }

    walkRegion(m.regions.head)
