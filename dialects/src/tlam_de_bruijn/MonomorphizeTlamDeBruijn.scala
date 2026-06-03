package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.*
import scair.dialects.builtin.*
import scair.dialects.tlam_de_bruijn.*
import scala.collection.mutable

object MonomorphizeTlamDeBruijn:

  private def instAt(
      t: TypeAttribute,
      tyArg: TypeAttribute,
      depth: Int,
  ): TypeAttribute =
    val shiftedArg =
      if depth == 0 then tyArg
      else DBI.shift(depth, 0, tyArg)
    DBI.subst(depth, shiftedArg, t)

  private def collectTLambdas(
      mod: ModuleOp
  ): Map[Value[TypeAttribute], TLambda] =
    val buf = mutable.Map.empty[Value[TypeAttribute], TLambda]

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.operations.foreach { op =>
          op match
            case tl: TLambda => buf += (tl.res: Value[TypeAttribute]) -> tl
            case _           => ()
          op.regions.foreach(walkRegion)
        }
      }

    mod.regions.foreach(walkRegion)
    buf.toMap

  private def collectTApplies(mod: ModuleOp): Seq[TApply] =
    val out = mutable.ArrayBuffer.empty[TApply]

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.operations.foreach { op =>
          op match
            case ta: TApply => out += ta
            case _          => ()
          op.regions.foreach(walkRegion)
        }
      }

    mod.regions.foreach(walkRegion)
    out.toSeq

  /** Clone a region, specializing all TypeAttributes by inst(..., tyArg), while
    * remapping SSA values so operands inside the clone refer to cloned defs.
    */
  private def cloneRegionSpec(
      r: Region,
      tyArg: TypeAttribute,
      depth: Int,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Region =
    Region(r.blocks.map(b => cloneBlockSpec(b, tyArg, depth)))

  private def cloneBlockSpec(
      b: Block,
      tyArg: TypeAttribute,
      depth: Int,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Block =
    val newArgTypes: Seq[Attribute] =
      b.arguments.iterator.map { a =>
        a.typ match
          case t: TypeAttribute => instAt(t, tyArg, depth)
          case other            => other
      }.toSeq

    Block(
      argumentsTypes = newArgTypes,
      (newArgs: Iterable[Value[Attribute]]) =>
        valueMapper.addAll(b.arguments.zip(newArgs))
        b.operations.map(op => cloneOpSpec(op, tyArg, depth)),
    )

  private def mapOperand(
      v: Value[Attribute]
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Value[Attribute] =
    valueMapper.getOrElse(v, v)

  private def cloneOpSpec(
      op: Operation,
      tyArg: TypeAttribute,
      depth: Int,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Operation =
    op match
      case v: VLambda =>
        val newFunTyTA = instAt(v.res.typ, tyArg, depth)
        val newFunTy = newFunTyTA match
          case f: tlamFunType => f
          case other          =>
            sys
              .error(
                s"monomorphize: expected tlamFunType after inst, got $other"
              )

        val newRes = Result[tlamFunType](newFunTy)
        valueMapper += (v.res: Value[Attribute]) -> (newRes: Value[Attribute])

        val newBody = cloneRegionSpec(v.body, tyArg, depth)
        VLambda(newBody, newRes)

      case vr: VReturn =>
        val newV = mapOperand(vr.value).asInstanceOf[Value[TypeAttribute]]
        VReturn(newV)

      case va: VApply =>
        val newFun = mapOperand(va.fun).asInstanceOf[Value[TypeAttribute]]
        val newArg = mapOperand(va.arg).asInstanceOf[Value[TypeAttribute]]

        val newResTy = instAt(va.res.typ, tyArg, depth)
        val newRes = Result[TypeAttribute](newResTy)
        valueMapper += (va.res: Value[Attribute]) -> (newRes: Value[Attribute])

        VApply(newFun, newArg, newRes)

      case tl: TLambda =>
        val newForAllTA = instAt(tl.res.typ, tyArg, depth)
        val newForAll = newForAllTA match
          case fa: tlamForAllType => fa
          case other              =>
            sys
              .error(
                s"monomorphize: expected tlamForAllType after inst, got $other"
              )

        val newRes = Result[tlamForAllType](newForAll)
        valueMapper += (tl.res: Value[Attribute]) -> (newRes: Value[Attribute])

        val newBody = cloneRegionSpec(tl.body, tyArg, depth + 1)
        TLambda(newBody, newRes)

      case tr: TReturn =>
        val newV = mapOperand(tr.value).asInstanceOf[Value[TypeAttribute]]
        TReturn(newV)

      case ta: TApply =>
        val newFun = mapOperand(ta.fun).asInstanceOf[Value[TypeAttribute]]
        val newTyArg = ta.tyArg match
          case t: TypeAttribute => instAt(t, tyArg, depth)
          case other            => other

        val newResTy = instAt(ta.res.typ, tyArg, depth)
        val newRes = Result[TypeAttribute](newResTy)
        valueMapper += (ta.res: Value[Attribute]) -> (newRes: Value[Attribute])

        TApply(newFun, newTyArg, newRes)

      case other =>
        val newOperands = other.operands.map(mapOperand)
        val newRegions = other.regions
          .map(r => cloneRegionSpec(r, tyArg, depth))

        val newResults: Seq[Result[Attribute]] =
          other.results.map { r =>
            val newTy: Attribute = r.typ match
              case t: TypeAttribute => instAt(t, tyArg, depth)
              case a                => a
            val nr = Result(newTy)
            valueMapper += (r: Value[Attribute]) -> (nr: Value[Attribute])
            nr
          }

        other.updated(
          operands = newOperands,
          results = newResults,
          regions = newRegions,
          successors = other.successors,
          properties = other.properties,
          attributes = other.attributes,
        )

  private def isEffectFreeForSpecialization(op: Operation): Boolean =
    op match
      case _: NoMemoryEffect                   => true
      case _: VLambda | _: TLambda | _: TApply =>
        true
      case _ =>
        false

  private def tlambdaPrefixIsEffectFree(tlam: TLambda): Boolean =
    tlam.body.blocks.headOption match
      case Some(bodyBlock) =>
        val bodyOps = bodyBlock.operations.toSeq
        bodyOps.nonEmpty && bodyOps.last.isInstanceOf[TReturn] &&
        bodyOps.dropRight(1).forall(isEffectFreeForSpecialization)
      case None =>
        false

  /** Rewrite of one TApply
    *   - clones the TLambda block ops (unattached) under specialization
    *   - inserts cloned ops (except final TReturn) before the TApply
    *   - replaces the TApply result with the cloned version of the returned
    *     value
    *
    * Returns the value that replaces the TApply (for memoization).
    */
  private def rewriteOneTApply(
      ta: TApply,
      tl: TLambda,
  ): Value[TypeAttribute] =
    val origBlock =
      tl.body.blocks.headOption
        .getOrElse {
          sys.error("monomorphize: tlambda has no blocks")
        }

    val origOps = origBlock.operations
    if origOps.isEmpty then
      sys.error("monomorphize: tlambda body block is empty")

    val retVal: Value[TypeAttribute] =
      origOps.last match
        case TReturn(v) => v
        case other      =>
          sys.error(
            s"monomorphize: tlambda terminator must be treturn, got ${other.name}"
          )

    given valueMapper: mutable.Map[Value[Attribute], Value[Attribute]] =
      mutable.Map.empty

    val clonedOpsUnattached: Seq[Operation] =
      origOps.toSeq.dropRight(1).map(op =>
        ta.tyArg match
          case t: TypeAttribute => cloneOpSpec(op, t, depth = 0)
          case other            =>
            sys.error(
              s"monomorphize: expected tapply type argument to be a TypeAttribute, got $other"
            )
      )

    val newRetAny =
      valueMapper.getOrElse(
        retVal.asInstanceOf[Value[Attribute]],
        sys
          .error(
            "monomorphize: return value not found in valueMapper (clone bug)"
          ),
      )

    val newRet = newRetAny.asInstanceOf[Value[TypeAttribute]]

    RewriteMethods.replaceOp(
      ta,
      newOps = clonedOpsUnattached,
      newResults = Some(Seq(newRet.asInstanceOf[Value[Attribute]])),
    )

    newRet

  def run(mod: ModuleOp): ModuleOp =
    val cache =
      mutable.Map
        .empty[(Block, Value[TypeAttribute], TypeAttribute), Value[
          TypeAttribute
        ]]

    var changed = true
    while changed do
      changed = false

      val tlByValue = collectTLambdas(mod)
      val tapplies = collectTApplies(mod)

      tapplies.foreach { ta =>
        ta.containerBlock
          .foreach { blk =>
            // If tyArg isn’t a TypeAttribute, skip (same as your old code).
            ta.tyArg match
              case tyArg: TypeAttribute =>
                tlByValue.get(ta.fun) match
                  case Some(tl) =>
                    val cacheable = tlambdaPrefixIsEffectFree(tl)

                    if cacheable then
                      cache.get((blk, ta.fun, tyArg)) match
                        case Some(existing) =>
                          // Replace tapply result with cached value and erase tapply.
                          RewriteMethods.replaceValue(
                            ta.res.asInstanceOf[Value[Attribute]],
                            existing.asInstanceOf[Value[Attribute]],
                          )
                          blk.eraseOp(ta)
                          changed = true

                        case None =>
                          val repl = rewriteOneTApply(ta, tl)
                          cache += (blk, ta.fun, tyArg) -> repl
                          tapplies.foreach { other =>
                            if (other ne ta) &&
                              other.containerBlock.contains(blk) &&
                              (other.fun eq ta.fun) &&
                              other.tyArg == tyArg
                            then
                              RewriteMethods.replaceValue(
                                other.res.asInstanceOf[Value[Attribute]],
                                repl.asInstanceOf[Value[Attribute]],
                              )
                              RewriteMethods.eraseOp(other, safeErase = false)
                          }
                          changed = true
                    else
                      // Not cacheable: always specialize fresh, don’t store/reuse cache.
                      val _ = rewriteOneTApply(ta, tl)
                      changed = true

                  case None =>
                    ()
              case _ =>
                ()
          }
      }

    mod

final class MonomorphizeTlamDeBruijnPass(ctx: MLContext)
    extends ModulePass(ctx):
  override val name: String = "monomorphize-tlam-de-bruijn"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp => MonomorphizeTlamDeBruijn.run(m)
      case other       => other
