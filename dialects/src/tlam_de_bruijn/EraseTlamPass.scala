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

  private def trailingTReturn(tl: TLambda): Option[TReturn] =
    tl.body.blocks.headOption.flatMap(_.operations.lastOption) match
      case Some(r: TReturn) => Some(r)
      case _                => None

  private def eraseInModule(m: ModuleOp): Unit =
    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        val ops = b.operations.toSeq
        ops.foreach {
          case tl: TLambda =>
            // Erase nested TLambda ops first, then erase this one.
            walkRegion(tl.body)
            // Be robust: if shape is malformed, leave untouched and let verifier
            // report it under --verify-diagnostics.
            trailingTReturn(tl) match
              case Some(tret) =>
                val bodyBlock = tl.body.blocks.head
                val bodyOps = bodyBlock.operations.toSeq

                val moved = bodyOps.dropRight(1).map(bodyBlock.detachOp)
                RewriteMethods.insertOpsBefore(tl, moved)

                RewriteMethods.replaceOp(
                  tl,
                  newOps = Seq.empty,
                  newResults = Some(Seq(tret.value)),
                )
              case None =>
                ()

          case other =>
            other.regions.foreach(walkRegion)
        }
      }

    walkRegion(m.regions.head)
