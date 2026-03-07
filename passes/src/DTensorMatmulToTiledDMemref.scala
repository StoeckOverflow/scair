package scair.passes.dtensor_matmul_to_tiled_dmemref

import scair.MLContext
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
      step: Value[Attribute],
      prelude: Seq[Operation],
      chosenTile: Int,
  )

  private def asNat(v: Value[Attribute]): Operand[dTensorNatType] =
    v.asInstanceOf[Operand[dTensorNatType]]

  private def asMemref(v: Value[Attribute]): Operand[d_memref.dMemrefMemrefType] =
    v.asInstanceOf[Operand[d_memref.dMemrefMemrefType]]

  private def asI32(v: Value[Attribute]): Operand[arith.AnyIntegerType] =
    v.asInstanceOf[Operand[arith.AnyIntegerType]]

  private def natConst(v: Int): NatConst =
    NatConst(IntegerAttr(IntData(v), I32), Result(dTensorNatType()))

  private def i32Const(v: Int): arith.Constant =
    arith.Constant(IntegerAttr(IntData(v), I32), Result(I32))

  private def chooseAxisPlan(
      dim: Value[Attribute],
      facts: NatDivisibilityFacts,
  ): AxisPlan =
    TileSizeChooser.chooseLargestGuaranteed(facts, dim) match
      case Some(tile) if tile > 1 =>
        val c = natConst(tile)
        AxisPlan(
          mode = AxisMode.TailFreeTiled,
          step = c.res,
          prelude = Seq(c),
          chosenTile = tile,
        )
      case _ =>
        // Explicit untiled fallback: step by full dimension.
        AxisPlan(
          mode = AxisMode.UntiledFallback,
          step = dim,
          prelude = Seq.empty,
          chosenTile = 1,
        )

  private def mkFor(
      lb: Value[Attribute],
      ub: Value[Attribute],
      step: Value[Attribute],
  )(
      bodyBuilder: Value[Attribute] => Seq[Operation]
  ): d_affine.For =
    val body = Region(
      Block(dTensorNatType(), iv => bodyBuilder(iv) :+ d_affine.Yield())
    )
    d_affine.For(asNat(lb), asNat(ub), asNat(step), body)

  private def mkSubview2D(
      src: Value[Attribute],
      off0: Value[Attribute],
      off1: Value[Attribute],
      size0: Value[Attribute],
      size1: Value[Attribute],
      elem: TypeAttribute,
  ): d_memref.Subview =
    val resTy = d_memref.dMemrefMemrefType(
      Seq(ValueAttribute(size0), ValueAttribute(size1)),
      elem,
    )
    d_memref.Subview(
      asMemref(src),
      Seq(asNat(off0), asNat(off1)),
      Seq(asNat(size0), asNat(size1)),
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

    val mPlan = chooseAxisPlan(mDim, facts)
    val nPlan = chooseAxisPlan(nDim, facts)
    val kPlan = chooseAxisPlan(kDim, facts)

    val lhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(lhsTy)
    val rhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(rhsTy)
    val outMemTy = DTensorDMemrefConversion.tensorToMemrefType(resTy)

    val (lhsPrefix, lhsMemV) = DTensorDMemrefConversion.toMemrefValue(mm.lhs, lhsMemTy)
    val (rhsPrefix, rhsMemV) = DTensorDMemrefConversion.toMemrefValue(mm.rhs, rhsMemTy)

    val outAlloc = d_memref.Alloc(Result(outMemTy))
    val n0 = natConst(0)
    val n1 = natConst(1)
    val c0 = i32Const(0)

    val outerI = mkFor(n0.res, mDim, mPlan.step) { ii =>
      val outerJ = mkFor(n0.res, nDim, nPlan.step) { jj =>
        val cTile =
          mkSubview2D(outAlloc.res, ii, jj, mPlan.step, nPlan.step, I32)

        val initI = mkFor(n0.res, mPlan.step, n1.res) { i =>
          Seq(
            mkFor(n0.res, nPlan.step, n1.res) { j =>
              Seq(
                d_memref.Store(
                  c0.result.asInstanceOf[Operand[TypeAttribute]],
                  asMemref(cTile.res),
                  Seq(asNat(i), asNat(j)),
                )
              )
            }
          )
        }

        val outerK = mkFor(n0.res, kDim, kPlan.step) { kk =>
          val aTile = mkSubview2D(lhsMemV, ii, kk, mPlan.step, kPlan.step, I32)
          val bTile = mkSubview2D(rhsMemV, kk, jj, kPlan.step, nPlan.step, I32)

          val compI = mkFor(n0.res, mPlan.step, n1.res) { i =>
            Seq(
              mkFor(n0.res, nPlan.step, n1.res) { j =>
                Seq(
                  mkFor(n0.res, kPlan.step, n1.res) { k =>
                    val la = d_memref.Load(
                      asMemref(aTile.res),
                      Seq(asNat(i), asNat(k)),
                      Result(I32),
                    )
                    val lb = d_memref.Load(
                      asMemref(bTile.res),
                      Seq(asNat(k), asNat(j)),
                      Result(I32),
                    )
                    val lc = d_memref.Load(
                      asMemref(cTile.res),
                      Seq(asNat(i), asNat(j)),
                      Result(I32),
                    )
                    val mul = arith.MulI(asI32(la.res), asI32(lb.res), Result(I32))
                    val add = arith.AddI(asI32(lc.res), asI32(mul.result), Result(I32))
                    val st = d_memref.Store(
                      add.result.asInstanceOf[Operand[TypeAttribute]],
                      asMemref(cTile.res),
                      Seq(asNat(i), asNat(j)),
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
        case AxisMode.TailFreeTiled  => StringData("tail_free_tiled")
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
    val castBack = castBackBase

    val newOps: Seq[Operation] =
      lhsPrefix ++ rhsPrefix ++
        mPlan.prelude ++ nPlan.prelude ++ kPlan.prelude ++
        Seq(outAlloc, n0, n1, c0, outerI, castBack)

    RewriteMethods.replaceOp(mm, newOps, Some(Seq(castBack.outputs.head)))

  override def transform(op: Operation): Operation =
    val facts = NatDivisibilityFacts(op)
    collectMatmuls(op).foreach(lowerOne(_, facts))
    op
