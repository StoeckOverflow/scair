package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.*
import scair.dialects.builtin.*
import scair.dialects.tlam.*
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

object Monomorphize:

  private def clonePayload(payload: Any): Any =
    payload match
      case a: Attribute => cloneAttr(a)
      case xs: Seq[?]   => xs.map(clonePayload)
      case opt: Option[?] =>
        opt.map(clonePayload)
      case other => other

  private def rebuildAttr(
      attr: Attribute,
      payloadMapper: Any => Any,
  ): Attribute =
      attr match
      case va: ValueAttribute =>
        new ValueAttribute(va.getVal())
      case p: Product =>
        val ctorOpt =
          attr.getClass.getConstructors.find(_.getParameterCount == p.productArity)
        ctorOpt match
          case Some(ctor) =>
            val args =
              p.productIterator.map(payloadMapper).map(_.asInstanceOf[Object]).toArray
            ctor.newInstance(args*).asInstanceOf[Attribute]
          case None =>
            attr
      case _ =>
        attr

  private def cloneAttr(a: Attribute): Attribute =
    a match
      case _: ParametrizedAttribute => rebuildAttr(a, clonePayload)
      case va: ValueAttribute       => new ValueAttribute(va.getVal())
      case other                    => other

  private def instPayload(
      payload: Any,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  ): Any =
    payload match
      case a: Attribute => instAttr(a, binderOpt, tyArg)
      case xs: Seq[?]   => xs.map(instPayload(_, binderOpt, tyArg))
      case opt: Option[?] =>
        opt.map(instPayload(_, binderOpt, tyArg))
      case other => other

  private def remapAttrUsesInPlace(
      a: Attribute
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Unit =
    AttributeWalker.remapTypeUsesInPlace(a)

  private def instAttr(
      a: Attribute,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  ): Attribute =
    a match
      case t: TypeAttribute => inst(t, binderOpt, tyArg)
      case _: ParametrizedAttribute =>
        rebuildAttr(a, instPayload(_, binderOpt, tyArg))
      case va: ValueAttribute =>
        new ValueAttribute(va.getVal())
      case other =>
        other

  private def instAndRemapAttr(
      a: Attribute,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Attribute =
    val out = instAttr(a, binderOpt, tyArg)
    remapAttrUsesInPlace(out)
    out

  private def instAndRemapType(
      t: TypeAttribute,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): TypeAttribute =
    instAndRemapAttr(t, binderOpt, tyArg).asInstanceOf[TypeAttribute]

  private def copyAttributes(
      from: Operation,
      to: Operation,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Operation =
    from.attributes.foreach { case (k, v) =>
      to.attributes.update(k, instAndRemapAttr(v, binderOpt, tyArg))
    }
    to

  /** Substitute both:
    *   - de Bruijn bvar(0) (for forall bodies) via DBI.subst
    *   - SSA type vars !value<%binder> via substTVar
    */
  private def inst(
      t: TypeAttribute,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  ): TypeAttribute =
    val t1 = DBI.subst(0, tyArg, t)
    binderOpt match
      case None         => t1
      case Some(binder) => substTVar(t1, binder, tyArg)

  /** Replace occurrences of !value<%binder> inside a TypeAttribute. */
  private def substTVar(
      t: TypeAttribute,
      binder: Value[Attribute],
      tyArg: TypeAttribute,
  ): TypeAttribute =
    t match
      case tv: ValueRefType if tv.value == binder =>
        cloneAttr(tyArg).asInstanceOf[TypeAttribute]

      case tv: ValueRefType =>
        ValueRefType(new ValueAttribute(tv.value))

      case TlamFunType(in, out) =>
        TlamFunType(
          substTVar(in, binder, tyArg),
          substTVar(out, binder, tyArg),
        )

      case TlamForAllType(body) =>
        // forall binds de Bruijn indices, not SSA binder values
        TlamForAllType(substTVar(body, binder, tyArg))

      case _: ParametrizedAttribute =>
        rebuildAttr(t, instPayload(_, Some(binder), tyArg)).asInstanceOf[TypeAttribute]

      case other =>
        other

  private def replaceAllUsesWith(
      from: Value[Attribute],
      to: Value[Attribute],
  ): Unit =
    val typeUsesSnapshot = from.typeUses.toList
    typeUsesSnapshot.foreach { tu =>
      from.typeUses -= tu
      tu.attribute.replaceValue(from, to)
      val v = tu.attribute.getVal()
      v.typeUses += TypeUse(tu.owner, tu.attribute)
    }

    val usesSnapshot = from.uses.toList

    val byOp: Map[Operation, List[Int]] =
      usesSnapshot.groupMap(_.operation)(_.index)

    byOp.foreach { case (userOp, indices0) =>
      val blkOpt = userOp.containerBlock

      blkOpt.foreach { blk =>
        val indices = indices0.distinct
        val newOperands =
          indices.foldLeft(userOp.operands)((ops, idx) => ops.updated(idx, to))

        val newUserOp =
          userOp.updated(
            operands = newOperands,
            successors = userOp.successors,
            results = userOp.results,
            regions = userOp.detachedRegions,
            properties = userOp.properties,
            attributes = userOp.attributes,
          )

        blk.insertOpBefore(userOp, newUserOp)
        blk.eraseOp(userOp, safeErase = false)
      }
    }

  private def collectTLambdas(
      mod: ModuleOp
  ): Map[Value[TlamForAllType], TLambda] =
    val buf = mutable.Map.empty[Value[TlamForAllType], TLambda]

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.operations.foreach { op =>
          op match
            case tl: TLambda => buf += (tl.res: Value[TlamForAllType]) -> tl
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

  private def opUsesValue(
      op: Operation,
      target: Value[Attribute],
  ): Boolean =
    op.operands.exists(_ eq target) ||
    op.regions.exists(_.blocks.exists(_.operations.exists(opUsesValue(_, target))))

  /** Clone a region, specializing all TypeAttributes by inst(...), while
    * remapping SSA values so operands inside the clone refer to cloned defs.
    */
  private def cloneRegionSpec(
      r: Region,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Region =
    Region(r.blocks.map(b => cloneBlockSpec(b, binderOpt, tyArg)))

  private def cloneBlockSpec(
      b: Block,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Block =
    val newArgTypes: Seq[Attribute] =
      b.arguments.iterator.map { a =>
        a.typ match
          case t: TypeAttribute => instAndRemapType(t, binderOpt, tyArg)
          case other            => instAndRemapAttr(other, binderOpt, tyArg)
      }.toSeq

    Block(
      argumentsTypes = newArgTypes,
      (newArgs: Iterable[Value[Attribute]]) =>
        valueMapper.addAll(b.arguments.zip(newArgs))
        b.operations.map(op => cloneOpSpec(op, binderOpt, tyArg)),
    )

  private def mapOperand(
      v: Value[Attribute]
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Value[Attribute] =
    valueMapper.getOrElse(v, v)

  private def cloneOpSpec(
      op: Operation,
      binderOpt: Option[Value[Attribute]],
      tyArg: TypeAttribute,
  )(using
      valueMapper: mutable.Map[Value[Attribute], Value[Attribute]]
  ): Operation =
    op match
      case v: VLambda =>
        val newFunTyTA = inst(v.res.typ, binderOpt, tyArg)
        val newFunTy = newFunTyTA match
          case f: TlamFunType => f
          case other          =>
            sys
              .error(
                s"monomorphize: expected TlamFunType after inst, got $other"
              )

        val newRes = Result[TlamFunType](newFunTy)
        remapAttrUsesInPlace(newRes.typ)
        valueMapper += (v.res: Value[Attribute]) -> (newRes: Value[Attribute])

        val newBody = cloneRegionSpec(v.body, binderOpt, tyArg)
        copyAttributes(v, VLambda(newBody, newRes), binderOpt, tyArg)

      case vr: VReturn =>
        val newV = mapOperand(vr.value).asInstanceOf[Value[TypeAttribute]]
        copyAttributes(vr, VReturn(newV), binderOpt, tyArg)

      case va: VApply =>
        val newFun = mapOperand(va.fun).asInstanceOf[Value[TlamFunType]]
        val newArg = mapOperand(va.arg).asInstanceOf[Value[TypeAttribute]]

        val newResTy = instAndRemapType(va.res.typ, binderOpt, tyArg)
        val newRes = Result[TypeAttribute](newResTy)
        valueMapper += (va.res: Value[Attribute]) -> (newRes: Value[Attribute])

        copyAttributes(va, VApply(newFun, newArg, newRes), binderOpt, tyArg)

      case tl: TLambda =>
        val newForAllTA = inst(tl.res.typ, binderOpt, tyArg)
        val newForAll = newForAllTA match
          case fa: TlamForAllType => fa
          case other              =>
            sys
              .error(
                s"monomorphize: expected TlamForAllType after inst, got $other"
              )

        val newRes = Result[TlamForAllType](newForAll)
        remapAttrUsesInPlace(newRes.typ)
        valueMapper += (tl.res: Value[Attribute]) -> (newRes: Value[Attribute])

        val newBody = cloneRegionSpec(tl.body, binderOpt, tyArg)
        copyAttributes(tl, TLambda(newBody, newRes), binderOpt, tyArg)

      case tr: TReturn =>
        val newV = mapOperand(tr.value).asInstanceOf[Value[TypeAttribute]]
        copyAttributes(tr, TReturn(newV), binderOpt, tyArg)

      case ta: TApply =>
        val newFun = mapOperand(ta.fun).asInstanceOf[Value[TlamForAllType]]
        val newTyArg = instAndRemapType(ta.tyArg, binderOpt, tyArg)

        val newResTy = instAndRemapType(ta.res.typ, binderOpt, tyArg)
        val newRes = Result[TypeAttribute](newResTy)
        valueMapper += (ta.res: Value[Attribute]) -> (newRes: Value[Attribute])

        copyAttributes(ta, TApply(newFun, newTyArg, newRes), binderOpt, tyArg)

      case other =>
        val newOperands = other.operands.map(mapOperand)
        val newRegions = other.regions
          .map(r => cloneRegionSpec(r, binderOpt, tyArg))

        val newResults: Seq[Result[Attribute]] =
          other.results.map { r =>
            val newTy: Attribute = r.typ match
              case t: TypeAttribute => instAndRemapType(t, binderOpt, tyArg)
              case a                => instAndRemapAttr(a, binderOpt, tyArg)
            val nr = Result(newTy)
            valueMapper += (r: Value[Attribute]) -> (nr: Value[Attribute])
            nr
          }

        val newProperties =
          other.properties.view
            .mapValues(instAndRemapAttr(_, binderOpt, tyArg))
            .toMap
        val newAttributes =
          LinkedHashMap.from(
            other.attributes.view
              .mapValues(instAndRemapAttr(_, binderOpt, tyArg))
          )

        other.updated(
          operands = newOperands,
          results = newResults,
          regions = newRegions,
          successors = other.successors,
          properties = newProperties,
          attributes = newAttributes,
        )

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
  ): Option[Value[TypeAttribute]] =
    val origBlock = tl.body.blocks.headOption match
      case Some(b) => b
      case None    => return None

    // NEW: SSA binder (e.g. %T : !tlam.type) for !value<%T> substitution.
    // If older IR exists without a binder arg, this gracefully disables SSA substitution.
    val binderOpt: Option[Value[Attribute]] =
      origBlock.arguments.headOption

    val origOps = origBlock.operations
    if origOps.isEmpty then return None

    val retVal: Value[TypeAttribute] =
      origOps.last match
        case TReturn(v) => v
        case _          => return None

    val useBlock = ta.containerBlock match
      case Some(b) => b
      case None    => return None

    given valueMapper: mutable.Map[Value[Attribute], Value[Attribute]] =
      mutable.Map.empty

    val unsupportedBinderOperandUse =
      binderOpt.exists { binder =>
      ta.tyArg match
        case tv: ValueRefType =>
          valueMapper += (binder -> tv.value)
          false
        case _ =>
          origOps.dropRight(1).exists(opUsesValue(_, binder)) ||
            (retVal.asInstanceOf[Value[Attribute]] eq binder)
      }
    if unsupportedBinderOperandUse then return None

    val clonedOpsUnattached: Seq[Operation] =
      origOps.toSeq.dropRight(1).map(op => cloneOpSpec(op, binderOpt, ta.tyArg))

    clonedOpsUnattached.foreach(op => useBlock.insertOpBefore(ta, op))

    val newRet =
      valueMapper.get(retVal.asInstanceOf[Value[Attribute]]) match
        case Some(v) => v.asInstanceOf[Value[TypeAttribute]]
        case None if binderOpt.contains(retVal.asInstanceOf[Value[Attribute]]) =>
          return None
        case None =>
          retVal

    replaceAllUsesWith(
      ta.res.asInstanceOf[Value[Attribute]],
      newRet.asInstanceOf[Value[Attribute]],
    )

    useBlock.eraseOp(ta)
    Some(newRet)

  def run(mod: ModuleOp): ModuleOp =
    val cache =
      mutable.Map
        .empty[(Block, Value[TlamForAllType], TypeAttribute), Value[
          TypeAttribute
        ]]

    var changed = true
    while changed do
      changed = false

      val tlByValue = collectTLambdas(mod)
      val tapplies = collectTApplies(mod)

      tapplies.foreach { ta =>
        ta.containerBlock.foreach { blk =>
          cache.get((blk, ta.fun, ta.tyArg)) match
            case Some(existing) =>
              replaceAllUsesWith(
                ta.res.asInstanceOf[Value[Attribute]],
                existing.asInstanceOf[Value[Attribute]],
              )
              blk.eraseOp(ta)
              changed = true

            case None =>
              tlByValue.get(ta.fun) match
                case Some(tl) =>
                  rewriteOneTApply(ta, tl).foreach { repl =>
                    cache += (blk, ta.fun, ta.tyArg) -> repl
                    changed = true
                    if tl.res.uses.isEmpty then RewriteMethods.eraseOp(tl)
                  }
                case None =>
                  ()
        }
        }
    collectTApplies(mod).headOption.foreach { _ =>
      throw new Exception(
        "monomorphize: unresolved tapply remained; callee must resolve to a clonable tlam.tlambda body"
      )
    }
    mod

final class MonomorphizePass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "monomorphize"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp => Monomorphize.run(m)
      case other       => other
