package scair.passes

import scair.MLContext
import scair.ir.*
import scair.dialects.tlam_de_bruijn.*
import scair.dialects.builtin.*
import scair.transformations.{ModulePass, RewriteMethods}

import scala.collection.mutable

/** Conservative value-level beta reduction for DB-only TLam: vapply(vlambda,
  * arg) -> inline vlambda body with block-arg mapped to arg.
  *
  * Safety policy:
  *   - only direct callee producers (fun.owner is a VLambda op),
  *   - only when cloned lambda body ops are all NoMemoryEffect.
  */
final class BetaReduceTLamPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "beta-reduce-tlam"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        reduceInModule(m)
        m
      case other => other

  private def reduceInModule(m: ModuleOp): Unit =
    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        val snapshot = b.operations.toList
        snapshot.foreach { op =>
          op.regions.foreach(walkRegion)
          op match
            case app: VApply => tryReduce(app)
            case _           => ()
        }
      }
    m.regions.foreach(walkRegion)

  private def tryReduce(app: VApply): Unit =
    app.fun.owner match
      case Some(vl: VLambda) =>
        if !isReducibleShape(vl) then return

        val lamBlock = vl.body.blocks.head
        val bodyOps = lamBlock.operations.toList
        val nonTermOps = bodyOps.dropRight(1)
        val ret = bodyOps.last.asInstanceOf[VReturn]
        val blockArg = lamBlock.arguments.head

        // Conservative rule: only clone side-effect-free bodies.
        if !nonTermOps.forall(isMemoryEffectFree) then return

        // Extra conservative guard: if the argument is effectful and the lambda
        // consumes its parameter multiple times, do not reduce.
        app.arg.owner match
          case Some(op: Operation) =>
            if !isMemoryEffectFree(op) && countUsesInLambda(blockArg, vl) > 1
            then return
          case _ => false

        given valueMapper: mutable.Map[Value[Attribute], Value[Attribute]] =
          mutable.Map.empty
        valueMapper += (blockArg: Value[Attribute]) ->
          (app.arg: Value[Attribute])

        val clonedOps = nonTermOps.map(_.deepCopy.asInstanceOf[Operation])

        val mappedRet = valueMapper.getOrElse(
          ret.value: Value[Attribute],
          ret.value: Value[Attribute],
        )

        app.containerBlock match
          case Some(_) =>
            RewriteMethods.replaceOp(
              app,
              newOps = clonedOps,
              newResults = Some(Seq(mappedRet)),
            )
          case None => ()
      case _ => ()

  private def isReducibleShape(vl: VLambda): Boolean =
    vl.body.blocks match
      case Block(args, ops) :: Nil =>
        args.length == 1 && ops.nonEmpty && ops.last.isInstanceOf[VReturn]
      case _ => false

  private def isMemoryEffectFree(op: Operation): Boolean =
    op.isInstanceOf[NoMemoryEffect] && op.regions.forall(r =>
      r.blocks.forall(b => b.operations.forall(isMemoryEffectFree))
    )

  private def countUsesInLambda(
      v: Value[Attribute],
      vl: VLambda,
  ): Int =
    v.uses.count(use => vl.isAncestor(use.operation))
