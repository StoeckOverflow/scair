package scair.passes.d_affine_loop_invariant_code_motion

import scair.MLContext
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.licm_helpers.DAffineForLoopAdapter
import scair.passes.licm_helpers.LoopInvariantCodeMotion
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

import scala.collection.mutable

private def collectLoopsInnermostFirst(op: Operation): Seq[d_affine.For] =
  val loops = mutable.ArrayBuffer.empty[d_affine.For]

  def visit(op: Operation): Unit =
    op.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
    op match
      case loop: d_affine.For => loops += loop
      case _                  => ()

  visit(op)
  loops.toSeq

private def tryHoist(loop: d_affine.For): Boolean =
  val adapter = DAffineForLoopAdapter(loop)
  val hoistable = LoopInvariantCodeMotion.findHoistableTopLevelOps(adapter)
  if hoistable.isEmpty then false
  else
    val (hoistedOps, rebuiltLoop) = adapter.rebuildWithHoisted(hoistable)
    RewriteMethods.replaceOp(loop, hoistedOps :+ rebuiltLoop, None)
    true

// Hoists loop-invariant pure arithmetic from d_affine loops in innermost-first
// order. This is a conservative LICM for refined layout arithmetic: it only
// moves side-effect-free arithmetic ops with operands defined outside the loop.
final class DAffineLoopInvariantCodeMotion(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "d-affine-loop-invariant-code-motion"

  override def transform(op: Operation): Operation =
    var changed = true
    while changed do
      changed = false
      collectLoopsInnermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryHoist(loop) then changed = true
      }
    op
