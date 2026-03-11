package scair.passes.d_linalg_to_d_affine

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.dialects.d_linalg
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asMemref(v: Value[Attribute]): Operand[d_memref.dMemrefMemrefType] =
  v.asInstanceOf[Operand[d_memref.dMemrefMemrefType]]

private def toIndex(nat: Value[Attribute]): ShapeToIndex =
  ShapeToIndex(nat.asInstanceOf[Operand[dTensorNatType]], Result(IndexType()))

private def idxConst(v: Int): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def loopIdentityMap: AffineMapAttr =
  AffineMapAttr(
    AffineMap(
      dimensions = Seq("d0"),
      symbols = Seq.empty,
      affineExprs = Seq(AffineDimExpr("d0")),
    )
  )

private def mkFor(
    lb: Value[Attribute],
    ub: Value[Attribute],
)(
    bodyBuilder: Value[Attribute] => Seq[Operation]
): d_affine.For =
  val body = Region(
    Block(IndexType(), iv => bodyBuilder(iv) :+ d_affine.Yield(Seq.empty))
  )
  d_affine.For(
    lowerBoundOperands = Seq(asIndex(lb)),
    upperBoundOperands = Seq(asIndex(ub)),
    inits = Seq.empty,
    res = Seq.empty,
    lowerBoundMap = loopIdentityMap,
    upperBoundMap = loopIdentityMap,
    step = IntegerAttr(IntData(1), I32),
    body = body,
  )

private def lowerFillNest(
    out: Value[Attribute],
    value: Operand[TypeAttribute],
    dims: Seq[Value[Attribute]],
    zero: Value[Attribute],
    ivs: Seq[Value[Attribute]] = Seq.empty,
): Seq[Operation] =
  dims match
    case Seq() =>
      Seq(
        d_memref.Store(
          value,
          asMemref(out),
          ivs.map(asIndex),
        )
      )
    case head +: tail =>
      Seq(
        mkFor(zero, head) { iv =>
          lowerFillNest(out, value, tail, zero, ivs :+ iv)
        }
      )

private def lowerScalarMulAdd(
    lhs: Value[Attribute],
    rhs: Value[Attribute],
    acc: Value[Attribute],
): Option[(Operation, Operation)] =
  acc.typ match
    case f: FloatType =>
      val mul = arith.MulF(
        lhs.asInstanceOf[Operand[FloatType]],
        rhs.asInstanceOf[Operand[FloatType]],
        Result(f),
        arith.FastMathFlagsAttr(arith.FastMathFlags.none),
      )
      val add = arith.AddF(
        acc.asInstanceOf[Operand[FloatType]],
        mul.result.asInstanceOf[Operand[FloatType]],
        Result(f),
        arith.FastMathFlagsAttr(arith.FastMathFlags.none),
      )
      Some((mul, add))
    case i: IntegerType =>
      val mul = arith.MulI(
        lhs.asInstanceOf[Operand[arith.AnyIntegerType]],
        rhs.asInstanceOf[Operand[arith.AnyIntegerType]],
        Result(i),
      )
      val add = arith.AddI(
        acc.asInstanceOf[Operand[arith.AnyIntegerType]],
        mul.result.asInstanceOf[Operand[arith.AnyIntegerType]],
        Result(i),
      )
      Some((mul, add))
    case _ => None

private val LowerFill = pattern {
  case d_linalg.Fill(value, out, res) if out.typ.isInstanceOf[d_memref.dMemrefMemrefType] && res.isEmpty =>
    val outTy = out.typ.asInstanceOf[d_memref.dMemrefMemrefType]
    val idxDims = outTy.params.map(_.getVal()).map(toIndex)
    val zero = idxConst(0)
    Seq(zero) ++ idxDims ++ lowerFillNest(out, value, idxDims.map(_.res), zero.result)
}

private val LowerMatmul = pattern {
  case d_linalg.Matmul(lhs, rhs, out, res)
      if lhs.typ.isInstanceOf[d_memref.dMemrefMemrefType] &&
        rhs.typ.isInstanceOf[d_memref.dMemrefMemrefType] &&
        out.typ.isInstanceOf[d_memref.dMemrefMemrefType] &&
        res.isEmpty =>
    val lhsTy = lhs.typ.asInstanceOf[d_memref.dMemrefMemrefType]
    val rhsTy = rhs.typ.asInstanceOf[d_memref.dMemrefMemrefType]
    val outTy = out.typ.asInstanceOf[d_memref.dMemrefMemrefType]
    val mIdx = toIndex(outTy.params(0).getVal())
    val nIdx = toIndex(outTy.params(1).getVal())
    val kIdx = toIndex(lhsTy.params(1).getVal())
    val zero = idxConst(0)
    val body =
      mkFor(zero.result, mIdx.res) { i =>
        Seq(
          mkFor(zero.result, nIdx.res) { j =>
            Seq(
              mkFor(zero.result, kIdx.res) { k =>
                val loadLhs = d_memref.Load(
                  asMemref(lhs),
                  Seq(asIndex(i), asIndex(k)),
                  Result(lhsTy.elem),
                )
                val loadRhs = d_memref.Load(
                  asMemref(rhs),
                  Seq(asIndex(k), asIndex(j)),
                  Result(rhsTy.elem),
                )
                val loadAcc = d_memref.Load(
                  asMemref(out),
                  Seq(asIndex(i), asIndex(j)),
                  Result(outTy.elem),
                )
                lowerScalarMulAdd(loadLhs.res, loadRhs.res, loadAcc.res) match
                  case Some((mul: Operation, add: Operation)) =>
                    Seq(
                      loadLhs,
                      loadRhs,
                      loadAcc,
                      mul,
                      add,
                      d_memref.Store(
                        add.results.head.asInstanceOf[Operand[TypeAttribute]],
                        asMemref(out),
                        Seq(asIndex(i), asIndex(j)),
                      ),
                    )
                  case None =>
                    throw new Exception(
                      s"lower-d-linalg-to-d-affine: unsupported matmul element type ${outTy.elem}"
                    )
              }
            )
          }
        )
      }
    Seq(zero, mIdx, nIdx, kIdx, body)
}

/**
 * Lowers structured `d_linalg` ops on `d_memref` buffers to explicit `d_affine`
 * loop nests.
 *
 * This pass rewrites buffer-based `d_linalg.fill` into nested loops that store the
 * fill value into every element, and rewrites buffer-based `d_linalg.matmul` into
 * a triple loop nest that performs `load/load/load -> mul -> add -> store`.
 *
 * Rewrite shapes:
 * `<d_linalg.fill %value, %out : !d.memref<...>>`
 * `->`
 * `<shape-to-index setup + nested d_affine.for + d_memref.store>`
 *
 * `<d_linalg.matmul %lhs, %rhs, %out : !d.memref<MxK>, !d.memref<KxN>, !d.memref<MxN>>`
 * `->`
 * `<shape-to-index setup + d_affine.for i/j/k + d_memref.load + arith.mul* + arith.add* + d_memref.store>`
 */
final class LowerDLinalgToDAffine(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "lower-d-linalg-to-d-affine"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(LowerFill, LowerMatmul))
  )
