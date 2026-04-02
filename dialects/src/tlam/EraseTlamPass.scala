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
    def attrRefsBinder(a: Attribute, binder: Value[Attribute]): Boolean =
      var found = false
      AttributeWalker.foreachValueAttribute(a) { va =>
        if va.getVal() eq binder then found = true
      }
      found

    def opRefsBinder(op: Operation, binder: Value[Attribute]): Boolean =
      op.operands.exists(_ eq binder) ||
      op.operands.exists(v => attrRefsBinder(v.typ, binder)) ||
      op.results.exists(r => attrRefsBinder(r.typ, binder)) ||
      op.attributes.values.exists(attrRefsBinder(_, binder)) ||
      op.properties.values.exists(attrRefsBinder(_, binder)) ||
      op.regions.exists(_.blocks.exists(_.operations.exists(opRefsBinder(_, binder))))

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
                    val binder = bodyBlock.arguments.head
                    val prefixOps = bodyOps.dropRight(1)
                    val binderLeaks =
                      prefixOps.exists(opRefsBinder(_, binder)) ||
                        attrRefsBinder(tret.value.typ, binder)
                    if !binderLeaks then
                      val moved = prefixOps.map(bodyBlock.detachOp)
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
