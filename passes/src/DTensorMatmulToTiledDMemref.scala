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

  private final case class AxisPlan(
      mode: AxisMode,
      loopUb: Value[Attribute],
      loopStep: IntegerAttr,
      tileSizeNat: Value[Attribute],
      tileSizeIdx: Value[Attribute],
      prelude: Seq[Operation],
      chosenTile: Int,
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

  private def lowerOne(
      mm: Matmul,
      facts: NatDivisibilityFacts,
  ): Unit =
    val lhsTy = mm.lhs.typ
    val rhsTy = mm.rhs.typ
    val resTy = mm.res.typ

    val isRank2 = lhsTy.params.size == 2 && rhsTy.params.size == 2 && resTy.params.size == 2
    val isI32Elem = lhsTy.elem == I32 && rhsTy.elem == I32 && resTy.elem == I32
    if !isRank2 || !isI32Elem then return

    val mDim = lhsTy.params(0).getVal()
    val kDim = lhsTy.params(1).getVal()
    val nDim = rhsTy.params(1).getVal()

    val mIdx = toIndex(mDim)
    val kIdx = toIndex(kDim)
    val nIdx = toIndex(nDim)
    val idx0 = idxConst(0)
    val idx1 = idxConst(1)

    val mPlan = chooseAxisPlan(mDim, mIdx.res, idx1.result, facts)
    val nPlan = chooseAxisPlan(nDim, nIdx.res, idx1.result, facts)
    val kPlan = chooseAxisPlan(kDim, kIdx.res, idx1.result, facts)

    val lhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(lhsTy)
    val rhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(rhsTy)
    val outMemTy = DTensorDMemrefConversion.tensorToMemrefType(resTy)

    val (lhsPrefix, lhsMemV) = DTensorDMemrefConversion.toMemrefValue(mm.lhs, lhsMemTy)
    val (rhsPrefix, rhsMemV) = DTensorDMemrefConversion.toMemrefValue(mm.rhs, rhsMemTy)

    val outAlloc = d_memref.Alloc(Result(outMemTy))
    val c0 = i32Const(0)

    val outerI = mkFor(idx0.result, mPlan.loopUb, mPlan.loopStep) { ii =>
      val iOff = mPlan.mode match
        case AxisMode.TailFreeTiled => ii
        case AxisMode.UntiledFallback => idx0.result

      val outerJ = mkFor(idx0.result, nPlan.loopUb, nPlan.loopStep) { jj =>
        val jOff = nPlan.mode match
          case AxisMode.TailFreeTiled => jj
          case AxisMode.UntiledFallback => idx0.result

        val cTile = mkSubview2D(
          outAlloc.res,
          iOff,
          jOff,
          mPlan.tileSizeNat,
          nPlan.tileSizeNat,
          mPlan.tileSizeIdx,
          nPlan.tileSizeIdx,
          idx1.result,
          I32,
        )

        val initI = mkFor(idx0.result, mPlan.tileSizeIdx, IntegerAttr(IntData(1), I32)) { i =>
          Seq(
            mkFor(idx0.result, nPlan.tileSizeIdx, IntegerAttr(IntData(1), I32)) { j =>
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

        val outerK = mkFor(idx0.result, kPlan.loopUb, kPlan.loopStep) { kk =>
          val kOff = kPlan.mode match
            case AxisMode.TailFreeTiled => kk
            case AxisMode.UntiledFallback => idx0.result

          val aTile = mkSubview2D(
            lhsMemV,
            iOff,
            kOff,
            mPlan.tileSizeNat,
            kPlan.tileSizeNat,
            mPlan.tileSizeIdx,
            kPlan.tileSizeIdx,
            idx1.result,
            I32,
          )
          val bTile = mkSubview2D(
            rhsMemV,
            kOff,
            jOff,
            kPlan.tileSizeNat,
            nPlan.tileSizeNat,
            kPlan.tileSizeIdx,
            nPlan.tileSizeIdx,
            idx1.result,
            I32,
          )

          val compI = mkFor(idx0.result, mPlan.tileSizeIdx, IntegerAttr(IntData(1), I32)) { i =>
            Seq(
              mkFor(idx0.result, nPlan.tileSizeIdx, IntegerAttr(IntData(1), I32)) { j =>
                Seq(
                  mkFor(idx0.result, kPlan.tileSizeIdx, IntegerAttr(IntData(1), I32)) { k =>
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
      outputs = Seq(Result(resTy)),
    )

    val modeStr = (m: AxisMode) =>
      m match
        case AxisMode.TailFreeTiled   => StringData("tail_free_tiled")
        case AxisMode.UntiledFallback => StringData("untiled_fallback")

    castBackBase.attributes.addOne("tile.m.mode" -> modeStr(mPlan.mode))
    castBackBase.attributes.addOne("tile.n.mode" -> modeStr(nPlan.mode))
    castBackBase.attributes.addOne("tile.k.mode" -> modeStr(kPlan.mode))
    castBackBase.attributes.addOne(
      "tile.m.value" -> IntegerAttr(IntData(mPlan.chosenTile), I32)
    )
    castBackBase.attributes.addOne(
      "tile.n.value" -> IntegerAttr(IntData(nPlan.chosenTile), I32)
    )
    castBackBase.attributes.addOne(
      "tile.k.value" -> IntegerAttr(IntData(kPlan.chosenTile), I32)
    )

    val newOps: Seq[Operation] =
      Seq(mIdx, kIdx, nIdx, idx0, idx1) ++
        lhsPrefix ++ rhsPrefix ++
        mPlan.prelude ++ nPlan.prelude ++ kPlan.prelude ++
        Seq(outAlloc, c0, outerI, castBackBase)

    RewriteMethods.replaceOp(mm, newOps, Some(Seq(castBackBase.outputs.head)))

  override def transform(op: Operation): Operation =
    val facts = NatDivisibilityFacts(op)
    collectMatmuls(op).foreach(lowerOne(_, facts))
    op
