package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.RewriteMethods
import scair.transformations.*
import scair.dialects.builtin.*
import scair.dialects.tlam.*

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
            // Erase is only sound once type-level application has been resolved
            // (typically by monomorphize). If TLambda is still used, keep it.
            if tl.res.uses.isEmpty && tl.res.typeUses.isEmpty then
              tl.body.blocks.headOption.foreach { bodyBlock =>
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
                  case _ =>
                    // Malformed TLambda: leave unchanged and let verifier report.
                    ()
              }

          case other =>
            other.regions.foreach(walkRegion)
        }
      }

    walkRegion(m.regions.head)
