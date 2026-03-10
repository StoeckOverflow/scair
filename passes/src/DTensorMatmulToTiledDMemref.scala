package scair.passes.dtensor_matmul_to_tiled_dmemref

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.builtin.UnrealizedConversionCastOp
import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.ir.*
import scair.passes.analysis.*
import scair.passes.dtensor_to_dmemref.DTensorDMemrefConversion
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

final class DTensorMatmulToTiledDMemref(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "dtensor-matmul-to-tiled-dmemref"

  private enum AxisMode:
    case TailFreeTiled, UntiledFallback

  private final case class MatmulMatch(
      mm: Matmul,
      lhsTy: dTensorTensorType,
      rhsTy: dTensorTensorType,
      resTy: dTensorTensorType,
      mDim: Value[Attribute],
      kDim: Value[Attribute],
      nDim: Value[Attribute],
  )

  private final case class AxisPlan(
      mode: AxisMode,
      loopUb: Value[Attribute],
      loopStep: IntegerAttr,
      tileSizeNat: Value[Attribute],
      tileSizeIdx: Value[Attribute],
      prelude: Seq[Operation],
      chosenTile: Int,
  )

  private final case class BridgedMatmul(
      matched: MatmulMatch,
      lhsMemTy: d_memref.dMemrefMemrefType,
      rhsMemTy: d_memref.dMemrefMemrefType,
      outMemTy: d_memref.dMemrefMemrefType,
      lhsPrefix: Seq[Operation],
      rhsPrefix: Seq[Operation],
      lhsMemV: Value[Attribute],
      rhsMemV: Value[Attribute],
      mIdx: ShapeToIndex,
      kIdx: ShapeToIndex,
      nIdx: ShapeToIndex,
      idx0: arith.Constant,
      idx1: arith.Constant,
  )

  private final case class MatmulTilingPlan(
      mPlan: AxisPlan,
      nPlan: AxisPlan,
      kPlan: AxisPlan,
  )

  private def asIndex(v: Value[Attribute]): Operand[IndexType] =
    v.asInstanceOf[Operand[IndexType]]

  private def asMemref(v: Value[Attribute]): Operand[d_memref.dMemrefMemrefType] =
    v.asInstanceOf[Operand[d_memref.dMemrefMemrefType]]

  private def asI32(v: Value[Attribute]): Operand[arith.AnyIntegerType] =
    v.asInstanceOf[Operand[arith.AnyIntegerType]]

  private def natConst(v: Int): NatConst =
    NatConst(IntegerAttr(IntData(v), I32), Result(dTensorNatType()))

  private def i32Const(v: Int): arith.Constant =
    arith.Constant(IntegerAttr(IntData(v), I32), Result(I32))

  private def idxConst(v: Int): arith.Constant =
    arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

  private def toIndex(nat: Value[Attribute]): ShapeToIndex =
    ShapeToIndex(nat.asInstanceOf[Operand[dTensorNatType]], Result(IndexType()))

  private def chooseAxisPlan(
      dimNat: Value[Attribute],
      dimIdx: Value[Attribute],
      oneIdx: Value[Attribute],
      facts: NatDivisibilityFacts,
  ): AxisPlan =
    TileSizeChooser.chooseLargestGuaranteedFromProvenance(facts, dimIdx) match
      case Some(tile) if tile > 1 =>
        val tileNat = natConst(tile)
        val tileIdx = toIndex(tileNat.res)
        AxisPlan(
          mode = AxisMode.TailFreeTiled,
          loopUb = dimIdx,
          loopStep = IntegerAttr(IntData(tile), I32),
          tileSizeNat = tileNat.res,
          tileSizeIdx = tileIdx.res,
          prelude = Seq(tileNat, tileIdx),
          chosenTile = tile,
        )
      case _ =>
        AxisPlan(
          mode = AxisMode.UntiledFallback,
          loopUb = oneIdx,
          loopStep = IntegerAttr(IntData(1), I32),
          tileSizeNat = dimNat,
          tileSizeIdx = dimIdx,
          prelude = Seq.empty,
          chosenTile = 1,
        )

  private def mkFor(
      lb: Value[Attribute],
      ub: Value[Attribute],
      step: IntegerAttr,
  )(
      bodyBuilder: Value[Attribute] => Seq[Operation]
  ): d_affine.For =
    val idMap = AffineMapAttr(
      AffineMap(
        dimensions = Seq("d0"),
        symbols = Seq.empty,
        affineExprs = Seq(AffineDimExpr("d0")),
      )
    )
    val body = Region(
      Block(IndexType(), iv => bodyBuilder(iv) :+ d_affine.Yield(Seq.empty))
    )
    d_affine.For(
      lowerBoundOperands = Seq(asIndex(lb)),
      upperBoundOperands = Seq(asIndex(ub)),
      inits = Seq.empty,
      res = Seq.empty,
      lowerBoundMap = idMap,
      upperBoundMap = idMap,
      step = step,
      body = body,
    )

  private def mkSubview2D(
      src: Value[Attribute],
      off0: Value[Attribute],
      off1: Value[Attribute],
      size0Nat: Value[Attribute],
      size1Nat: Value[Attribute],
      size0Idx: Value[Attribute],
      size1Idx: Value[Attribute],
      oneIdx: Value[Attribute],
      elem: TypeAttribute,
  ): d_memref.Subview =
    val resTy = d_memref.dMemrefMemrefType(
      Seq(ValueAttribute(size0Nat), ValueAttribute(size1Nat)),
      elem,
    )
    d_memref.Subview(
      asMemref(src),
      Seq(asIndex(off0), asIndex(off1)),
      Seq(asIndex(size0Idx), asIndex(size1Idx)),
      Seq(asIndex(oneIdx), asIndex(oneIdx)),
      Result(resTy),
    )

  private def collectMatmuls(op: Operation): Seq[Matmul] =
    val here = op match
      case m: Matmul => Seq(m)
      case _         => Seq.empty
    here ++ op.regions.flatMap(_.blocks).flatMap(_.operations).flatMap(collectMatmuls)

  private def matchSupported(mm: Matmul): Option[MatmulMatch] =
    val lhsTy = mm.lhs.typ
    val rhsTy = mm.rhs.typ
    val resTy = mm.res.typ

    val isRank2 = lhsTy.params.size == 2 && rhsTy.params.size == 2 && resTy.params.size == 2
    val isI32Elem = lhsTy.elem == I32 && rhsTy.elem == I32 && resTy.elem == I32
    if !isRank2 || !isI32Elem then None
    else
      Some(
        MatmulMatch(
          mm = mm,
          lhsTy = lhsTy,
          rhsTy = rhsTy,
          resTy = resTy,
          mDim = lhsTy.params(0).getVal(),
          kDim = lhsTy.params(1).getVal(),
          nDim = rhsTy.params(1).getVal(),
        )
      )

  private def bridgeMatmul(matched: MatmulMatch): BridgedMatmul =
    val mIdx = toIndex(matched.mDim)
    val kIdx = toIndex(matched.kDim)
    val nIdx = toIndex(matched.nDim)
    val idx0 = idxConst(0)
    val idx1 = idxConst(1)

    val lhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(matched.lhsTy)
    val rhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(matched.rhsTy)
    val outMemTy = DTensorDMemrefConversion.tensorToMemrefType(matched.resTy)

    val (lhsPrefix, lhsMemV) = DTensorDMemrefConversion.toMemrefValue(matched.mm.lhs, lhsMemTy)
    val (rhsPrefix, rhsMemV) = DTensorDMemrefConversion.toMemrefValue(matched.mm.rhs, rhsMemTy)

    BridgedMatmul(
      matched = matched,
      lhsMemTy = lhsMemTy,
      rhsMemTy = rhsMemTy,
      outMemTy = outMemTy,
      lhsPrefix = lhsPrefix,
      rhsPrefix = rhsPrefix,
      lhsMemV = lhsMemV,
      rhsMemV = rhsMemV,
      mIdx = mIdx,
      kIdx = kIdx,
      nIdx = nIdx,
      idx0 = idx0,
      idx1 = idx1,
    )

  private def planTiling(
      bridged: BridgedMatmul,
      facts: NatDivisibilityFacts,
  ): MatmulTilingPlan =
    MatmulTilingPlan(
      mPlan = chooseAxisPlan(
        bridged.matched.mDim,
        bridged.mIdx.res,
        bridged.idx1.result,
        facts,
      ),
      nPlan = chooseAxisPlan(
        bridged.matched.nDim,
        bridged.nIdx.res,
        bridged.idx1.result,
        facts,
      ),
      kPlan = chooseAxisPlan(
        bridged.matched.kDim,
        bridged.kIdx.res,
        bridged.idx1.result,
        facts,
      ),
    )

  private def attachTileAttrs(
      castBackBase: UnrealizedConversionCastOp,
      plan: MatmulTilingPlan,
  ): Unit =
    val modeStr = (m: AxisMode) =>
      m match
        case AxisMode.TailFreeTiled   => StringData("tail_free_tiled")
        case AxisMode.UntiledFallback => StringData("untiled_fallback")

    castBackBase.attributes.addOne("tile.m.mode" -> modeStr(plan.mPlan.mode))
    castBackBase.attributes.addOne("tile.n.mode" -> modeStr(plan.nPlan.mode))
    castBackBase.attributes.addOne("tile.k.mode" -> modeStr(plan.kPlan.mode))
    castBackBase.attributes.addOne(
      "tile.m.value" -> IntegerAttr(IntData(plan.mPlan.chosenTile), I32)
    )
    castBackBase.attributes.addOne(
      "tile.n.value" -> IntegerAttr(IntData(plan.nPlan.chosenTile), I32)
    )
    castBackBase.attributes.addOne(
      "tile.k.value" -> IntegerAttr(IntData(plan.kPlan.chosenTile), I32)
    )

  private def emitTiledMatmul(
      bridged: BridgedMatmul,
      plan: MatmulTilingPlan,
  ): (Seq[Operation], Value[Attribute]) =
    val outAlloc = d_memref.Alloc(Result(bridged.outMemTy))
    val c0 = i32Const(0)

    val outerI = mkFor(bridged.idx0.result, plan.mPlan.loopUb, plan.mPlan.loopStep) { ii =>
      val iOff = plan.mPlan.mode match
        case AxisMode.TailFreeTiled   => ii
        case AxisMode.UntiledFallback => bridged.idx0.result

      val outerJ = mkFor(bridged.idx0.result, plan.nPlan.loopUb, plan.nPlan.loopStep) { jj =>
        val jOff = plan.nPlan.mode match
          case AxisMode.TailFreeTiled   => jj
          case AxisMode.UntiledFallback => bridged.idx0.result

        val cTile = mkSubview2D(
          outAlloc.res,
          iOff,
          jOff,
          plan.mPlan.tileSizeNat,
          plan.nPlan.tileSizeNat,
          plan.mPlan.tileSizeIdx,
          plan.nPlan.tileSizeIdx,
          bridged.idx1.result,
          I32,
        )

        val initI = mkFor(
          bridged.idx0.result,
          plan.mPlan.tileSizeIdx,
          IntegerAttr(IntData(1), I32),
        ) { i =>
          Seq(
            mkFor(
              bridged.idx0.result,
              plan.nPlan.tileSizeIdx,
              IntegerAttr(IntData(1), I32),
            ) { j =>
              Seq(
                d_memref.Store(
                  c0.result.asInstanceOf[Operand[TypeAttribute]],
                  asMemref(cTile.res),
                  Seq(asIndex(i), asIndex(j)),
                )
              )
            }
          )
        }

        val outerK = mkFor(bridged.idx0.result, plan.kPlan.loopUb, plan.kPlan.loopStep) { kk =>
          val kOff = plan.kPlan.mode match
            case AxisMode.TailFreeTiled   => kk
            case AxisMode.UntiledFallback => bridged.idx0.result

          val aTile = mkSubview2D(
            bridged.lhsMemV,
            iOff,
            kOff,
            plan.mPlan.tileSizeNat,
            plan.kPlan.tileSizeNat,
            plan.mPlan.tileSizeIdx,
            plan.kPlan.tileSizeIdx,
            bridged.idx1.result,
            I32,
          )
          val bTile = mkSubview2D(
            bridged.rhsMemV,
            kOff,
            jOff,
            plan.kPlan.tileSizeNat,
            plan.nPlan.tileSizeNat,
            plan.kPlan.tileSizeIdx,
            plan.nPlan.tileSizeIdx,
            bridged.idx1.result,
            I32,
          )

          val compI = mkFor(
            bridged.idx0.result,
            plan.mPlan.tileSizeIdx,
            IntegerAttr(IntData(1), I32),
          ) { i =>
            Seq(
              mkFor(
                bridged.idx0.result,
                plan.nPlan.tileSizeIdx,
                IntegerAttr(IntData(1), I32),
              ) { j =>
                Seq(
                  mkFor(
                    bridged.idx0.result,
                    plan.kPlan.tileSizeIdx,
                    IntegerAttr(IntData(1), I32),
                  ) { k =>
                    val la = d_memref.Load(
                      asMemref(aTile.res),
                      Seq(asIndex(i), asIndex(k)),
                      Result(I32),
                    )
                    val lb = d_memref.Load(
                      asMemref(bTile.res),
                      Seq(asIndex(k), asIndex(j)),
                      Result(I32),
                    )
                    val lc = d_memref.Load(
                      asMemref(cTile.res),
                      Seq(asIndex(i), asIndex(j)),
                      Result(I32),
                    )
                    val mul = arith.MulI(asI32(la.res), asI32(lb.res), Result(I32))
                    val add = arith.AddI(asI32(lc.res), asI32(mul.result), Result(I32))
                    val st = d_memref.Store(
                      add.result.asInstanceOf[Operand[TypeAttribute]],
                      asMemref(cTile.res),
                      Seq(asIndex(i), asIndex(j)),
                    )
                    Seq(la, lb, lc, mul, add, st)
                  }
                )
              }
            )
          }

          Seq(aTile, bTile, compI)
        }

        Seq(cTile, initI, outerK)
      }
      Seq(outerJ)
    }

    val castBackBase = UnrealizedConversionCastOp(
      inputs = Seq(outAlloc.res),
      outputs = Seq(Result(bridged.matched.resTy)),
    )
    attachTileAttrs(castBackBase, plan)

    val newOps: Seq[Operation] =
      Seq(
        bridged.mIdx,
        bridged.kIdx,
        bridged.nIdx,
        bridged.idx0,
        bridged.idx1,
      ) ++
        bridged.lhsPrefix ++ bridged.rhsPrefix ++
        plan.mPlan.prelude ++ plan.nPlan.prelude ++ plan.kPlan.prelude ++
        Seq(outAlloc, c0, outerI, castBackBase)

    (newOps, castBackBase.outputs.head)

  private def lowerOne(
      mm: Matmul,
      facts: NatDivisibilityFacts,
  ): Unit =
    matchSupported(mm).foreach { matched =>
      val bridged = bridgeMatmul(matched)
      val plan = planTiling(bridged, facts)
      val (newOps, result) = emitTiledMatmul(bridged, plan)
      RewriteMethods.replaceOp(mm, newOps, Some(Seq(result)))
    }

  override def transform(op: Operation): Operation =
    val facts = NatDivisibilityFacts(op)
    collectMatmuls(op).foreach(lowerOne(_, facts))
    op
