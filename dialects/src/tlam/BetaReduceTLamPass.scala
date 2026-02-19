package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.RewriteMethods
import scair.transformations.ModulePass
import scair.dialects.tlam.*
import scair.dialects.builtin.*
import scala.collection.mutable

/** Local value-level beta-reduction for the SSA-in-types TLam dialect.
  *
  * Rewrites:
  *   tlam.vapply (tlam.vlambda { ^bb0(%x): ... tlam.vreturn %v }) %arg
  * into cloned body ops inserted before vapply, with %x mapped to %arg.
  *
  * This pass is conservative:
  *   - callee must be a direct VLambda producer,
  *   - body ops (except final vreturn) must be pure by trait/pattern,
  *   - if the actual argument comes from an effectful producer and is used more
  *     than once in the lambda body, skip reduction.
  */
final class BetaReduceTLamPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "beta-reduce-tlam"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        run(m); m
      case other => other

  private def run(m: ModuleOp): Unit =
    var changed = true
    while changed do
      changed = false
      def walkRegion(r: Region): Unit =
        r.blocks.foreach { b =>
          val ops = b.operations.toSeq
          ops.foreach { op =>
            op match
              case app: VApply =>
                if betaReduce(app) then changed = true
              case _ =>
                ()
            if op.containerBlock.nonEmpty then op.regions.foreach(walkRegion)
          }
        }
      walkRegion(m.regions.head)

  private def isPureOp(op: Operation): Boolean =
    op match
      case _: NoMemoryEffect => true
      // TLam ops are pure by construction in this calculus.
      case _: VLambda | _: VApply | _: VReturn | _: TLambda | _: TApply |
          _: TReturn =>
        true
      case _ => false

  private def countValueUsesInOpTree(
      v: Value[Attribute],
      op: Operation,
  ): Int =
    var n = 0

    def bumpAttr(a: Attribute): Unit =
      AttributeWalker.foreachValueAttribute(a) { va =>
        if va.getVal() eq v then n += 1
      }

    def walk(o: Operation): Unit =
      o.operands.foreach { ov =>
        if ov eq v then n += 1
      }
      o.results.foreach(r => bumpAttr(r.typ))
      o.operands.foreach(ov => bumpAttr(ov.typ))
      o.attributes.values.foreach(bumpAttr)
      o.properties.values.foreach(bumpAttr)
      o.regions.foreach { rr =>
        rr.blocks.foreach(_.operations.foreach(walk))
      }

    walk(op)
    n

  private def betaReduce(app: VApply): Boolean =
    val lam =
      app.fun.owner match
        case Some(vl: VLambda) => vl
        case _                 => return false

    val lamBlock =
      lam.body.blocks match
        case Seq(bb) => bb
        case _       => return false

    if lamBlock.arguments.length != 1 then return false
    val param = lamBlock.arguments.head

    val bodyOps = lamBlock.operations.toSeq
    if bodyOps.isEmpty then return false
    val ret =
      bodyOps.last match
        case vr: VReturn => vr
        case _           => return false

    val prefixOps = bodyOps.dropRight(1)
    if !prefixOps.forall(isPureOp) then return false

    val paramUseCount = prefixOps.map(countValueUsesInOpTree(param, _)).sum +
      countValueUsesInOpTree(param, ret)

    val effectfulArgProducer =
      app.arg.owner match
        case Some(prod: Operation) => !isPureOp(prod)
        case _                     => false

    if effectfulArgProducer && paramUseCount > 1 then return false

    val useBlock = app.containerBlock match
      case Some(b) => b
      case None    => return false

    given valueMapper: mutable.Map[Value[Attribute], Value[Attribute]] =
      mutable.Map.empty

    valueMapper += (param -> app.arg)

    val cloned = prefixOps.map(_.deepCopy)
    RewriteMethods.insertOpsBefore(app, cloned)

    val mappedRet = valueMapper
      .getOrElse(
        ret.value.asInstanceOf[Value[Attribute]],
        ret.value.asInstanceOf[Value[Attribute]],
      )

    RewriteMethods.replaceValue(
      app.res.asInstanceOf[Value[Attribute]],
      mappedRet,
    )
    useBlock.eraseOp(app, safeErase = false)
    true
