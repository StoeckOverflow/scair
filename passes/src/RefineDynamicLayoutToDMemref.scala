package scair.passes.refine_dynamic_layout_to_dmemref

import scair.MLContext
import scair.dialects.affine
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.dialects.memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

private def i32Attr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), I32)

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def remapValue(
    value: Value[Attribute],
    valueMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Value[Attribute] =
  valueMapper.getOrElse(value, value)

// Region refinement preserves structured loop nesting while replacing baseline
// memref/affine operations with their refined d_memref/d_affine counterparts.
private def lowerRegion(
    region: Region,
    outerMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Region =
  val oldBlock = region.blocks.head
  Region(
    Block(oldBlock.arguments.map(_.typ), newArgs =>
      val localMapper = mutable.Map.from(outerMapper)
      localMapper.addAll(oldBlock.arguments.zip(newArgs))
      oldBlock.operations.flatMap { op =>
        val lowered: Seq[Operation] = op match
          case op: affine.For =>
            Seq(
              d_affine.For(
                op.lowerBoundOperands.map(v => remapValue(v, localMapper).asInstanceOf[Operand[IndexType]]),
                op.upperBoundOperands.map(v => remapValue(v, localMapper).asInstanceOf[Operand[IndexType]]),
                op.inits.map(v => remapValue(v, localMapper).asInstanceOf[Operand[Attribute]]),
                op.res.map(r => Result(r.typ)),
                op.lowerBoundMap,
                op.upperBoundMap,
                op.step,
                lowerRegion(op.body, localMapper),
              )
            )
          case op: affine.Yield =>
            Seq(d_affine.Yield(op.arguments.map(v => remapValue(v, localMapper).asInstanceOf[Operand[Attribute]])))
          case op: affine.Load if identityMap(op.map, op.indices.size) =>
            Seq(
              d_memref.Load(
                remapValue(op.memref, localMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
                op.indices.map(v => remapValue(v, localMapper).asInstanceOf[Operand[IndexType]]),
                Result(op.result.typ.asInstanceOf[TypeAttribute]),
              )
            )
          case op: affine.Store if identityMap(op.map, op.indices.size) =>
            Seq(
              d_memref.Store(
                remapValue(op.value, localMapper).asInstanceOf[Operand[TypeAttribute]],
                remapValue(op.memref, localMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
                op.indices.map(v => remapValue(v, localMapper).asInstanceOf[Operand[IndexType]]),
              )
            )
          case op: memref.Load =>
            Seq(
              d_memref.Load(
                remapValue(op.memref, localMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
                op.indices.map(v => remapValue(v, localMapper).asInstanceOf[Operand[IndexType]]),
                Result(op.result.typ.asInstanceOf[TypeAttribute]),
              )
            )
          case op: memref.Store =>
            Seq(
              d_memref.Store(
                remapValue(op.value, localMapper).asInstanceOf[Operand[TypeAttribute]],
                remapValue(op.memref, localMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
                op.indices.map(v => remapValue(v, localMapper).asInstanceOf[Operand[IndexType]]),
              )
            )
          case other =>
            Seq(other.deepCopy(using mutable.Map.empty, localMapper))
        lowered.lastOption.foreach { rewritten =>
          localMapper.addAll(op.results.zip(rewritten.results))
        }
        lowered
      }
    )
  )

private def identityMap(map: AffineMapAttr, rank: Int): Boolean =
  map.affineMap.affineExprs.size == rank &&
    map.affineMap.symbols.isEmpty &&
    map.affineMap.affineExprs.zipWithIndex.forall {
      case (affine.AffineDimExpr(name), i) => map.affineMap.dimensions.indexOf(name) == i
      case _                        => false
    }

private def staticNat(v: BigInt): (Seq[Operation], ValueAttribute) =
  val c = dTensor.NatConst(i32Attr(v), Result(dTensor.dTensorNatType()))
  (Seq(c), ValueAttribute(c.res))

// Dynamic dimensions are reified as nat values so that refined memref types can
// carry dimension information directly in the type.
private def dynamicNat(v: Operand[IndexType]): (Seq[Operation], ValueAttribute) =
  val cast = dTensor.IndexToNat(v, Result(dTensor.dTensorNatType()))
  (Seq(cast), ValueAttribute(cast.res))

private def refineBaseMemrefType(
    ty: RankedMemrefType,
    dynamicDims: Seq[Operand[IndexType]],
): (Seq[Operation], d_memref.dMemrefMemrefType) =
  var dynIdx = 0
  val emitted = scala.collection.mutable.ArrayBuffer.empty[Operation]
  val dims = ty.shape.attrValues.map { dim =>
    if dim.data >= 0 then
      val (ops, attr) = staticNat(dim.data)
      emitted ++= ops
      attr
    else
      val (ops, attr) = dynamicNat(dynamicDims(dynIdx))
      dynIdx += 1
      emitted ++= ops
      attr
  }
  (emitted.toSeq, d_memref.dMemrefMemrefType(dims, ty.elementType.asInstanceOf[TypeAttribute]))

// Reinterpret refinement preserves the explicit view parameters as refined
// layout parameters so later passes can reason about offset and stride values
// before descriptor materialization.
private def refineReinterpretType(
    ty: RankedMemrefType,
    sizes: Seq[Operand[IndexType]],
    offset: Operand[IndexType],
    strides: Seq[Operand[IndexType]],
): (Seq[Operation], d_memref.dMemrefMemrefType) =
  val emitted = scala.collection.mutable.ArrayBuffer.empty[Operation]
  var sizeIdx = 0
  val dims = ty.shape.attrValues.map { dim =>
    if dim.data >= 0 then
      val (ops, attr) = staticNat(dim.data)
      emitted ++= ops
      attr
    else
      val (ops, attr) = dynamicNat(sizes(sizeIdx))
      sizeIdx += 1
      emitted ++= ops
      attr
  }
  val layout = ty.encoding.collect { case s: StridedLayoutAttr => s }.get
  var strideIdx = 0
  val refinedStrides = layout.strides.attrValues.map { stride =>
    if stride.data >= 0 then IntegerAttr(IntData(stride.data), IndexType()): d_memref.LayoutParam
    else
      val v = strides(strideIdx)
      strideIdx += 1
      ValueAttribute(v)
  }
  val refinedOffset: d_memref.LayoutParam =
    ValueAttribute(offset)
  (
    emitted.toSeq,
    d_memref.dMemrefMemrefType(
      dims,
      ty.elementType.asInstanceOf[TypeAttribute],
      Some(refinedOffset),
      Some(refinedStrides),
    ),
  )

private val RefineAlloc = pattern {
  case op: memref.Alloc =>
    op.memref.typ match
      case ty: RankedMemrefType =>
        val (prefix, refinedTy) = refineBaseMemrefType(ty, op.dynamicSizes)
        val alloc = d_memref.Alloc(Result(refinedTy))
        (prefix :+ alloc, Seq(alloc.res))
      case _ =>
        PatternAction.Abort
}

private val RefineDealloc = pattern {
  case op: memref.Dealloc =>
    d_memref.Dealloc(op.memref.asInstanceOf[Operand[d_memref.dMemrefMemrefType]])
}

private val RefineLoad = pattern {
  case op: memref.Load =>
    d_memref.Load(
      op.memref.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      op.indices,
      Result(op.result.typ.asInstanceOf[TypeAttribute]),
    )
}

private val RefineStore = pattern {
  case op: memref.Store =>
    d_memref.Store(
      op.value.asInstanceOf[Operand[TypeAttribute]],
      op.memref.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      op.indices,
    )
}

private val RefineReinterpret = pattern {
  case op: memref.ReinterpretCast =>
    op.res.typ match
      case ty: RankedMemrefType if ty.encoding.exists(_.isInstanceOf[StridedLayoutAttr]) =>
        val (prefix, refinedTy) = refineReinterpretType(ty, op.sizes, op.offset, op.strides)
        val refined = d_memref.ReinterpretCast(
          op.src.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
          Result(refinedTy),
        )
        (prefix :+ refined, Seq(refined.res))
      case _ =>
        PatternAction.Abort
}

private val RefineAffineYield = pattern {
  case op: affine.Yield =>
    d_affine.Yield(op.arguments)
}

private val RefineAffineFor = pattern {
  case op: affine.For =>
    d_affine.For(
      op.lowerBoundOperands.map(_.asInstanceOf[Operand[IndexType]]),
      op.upperBoundOperands.map(_.asInstanceOf[Operand[IndexType]]),
      op.inits.map(_.asInstanceOf[Operand[Attribute]]),
      op.res.map(r => Result(r.typ)),
      op.lowerBoundMap,
      op.upperBoundMap,
      op.step,
      lowerRegion(op.body, mutable.Map.empty),
    )
}

private val RefineAffineLoad = pattern {
  case op: affine.Load if identityMap(op.map, op.indices.size) =>
    d_memref.Load(
      op.memref.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      op.indices,
      Result(op.result.typ.asInstanceOf[TypeAttribute]),
    )
}

private val RefineAffineStore = pattern {
  case op: affine.Store if identityMap(op.map, op.indices.size) =>
    d_memref.Store(
      op.value.asInstanceOf[Operand[TypeAttribute]],
      op.memref.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      op.indices,
    )
}

// Refines baseline memref/affine IR into d_memref/d_affine IR.
// Example: `memref.alloc` / `memref.reinterpret_cast` / `affine.for`
//   -> `d_memref.alloc` / `d_memref.reinterpret_cast` / `d_affine.for`,
//      with dynamic dimensions reified as refined type parameters.
final class RefineDynamicLayoutToDMemref(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "refine-dynamic-layout-to-dmemref"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(
          RefineAlloc,
          RefineReinterpret,
          RefineDealloc,
          RefineLoad,
          RefineStore,
          RefineAffineLoad,
          RefineAffineStore,
          RefineAffineYield,
          RefineAffineFor,
        )
      )
    )
