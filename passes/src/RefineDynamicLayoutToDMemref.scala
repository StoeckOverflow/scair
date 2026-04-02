package scair.passes.refine_dynamic_layout_to_dmemref

import scair.MLContext
import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.dialects.func
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

private def identityMap(map: AffineMapAttr, rank: Int): Boolean =
  map.affineMap.affineExprs.size == rank &&
    map.affineMap.symbols.isEmpty &&
    map.affineMap.affineExprs.zipWithIndex.forall {
      case (affine.AffineDimExpr(name), i) =>
        map.affineMap.dimensions.indexOf(name) == i
      case _ => false
    }

private def staticNat(v: BigInt): (Seq[Operation], ValueAttribute) =
  val c = dTensor.NatConst(i32Attr(v), Result(dTensor.dTensorNatType()))
  (Seq(c), ValueAttribute(c.res))

private def dynamicNat(v: Operand[IndexType]): (Seq[Operation], ValueAttribute) =
  val cast = dTensor.IndexToNat(v, Result(dTensor.dTensorNatType()))
  (Seq(cast), ValueAttribute(cast.res))

private def refineBaseMemrefType(
    ty: RankedMemrefType,
    dynamicDims: Seq[Operand[IndexType]],
): (Seq[Operation], d_memref.dMemrefMemrefType) =
  var dynIdx = 0
  val emitted = mutable.ArrayBuffer.empty[Operation]
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
  (
    emitted.toSeq,
    d_memref.dMemrefMemrefType(dims, ty.elementType.asInstanceOf[TypeAttribute]),
  )

private def refineReinterpretType(
    ty: RankedMemrefType,
    sizes: Seq[Operand[IndexType]],
    offset: Operand[IndexType],
    strides: Seq[Operand[IndexType]],
): (Seq[Operation], d_memref.dMemrefMemrefType) =
  val emitted = mutable.ArrayBuffer.empty[Operation]
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
    if stride.data >= 0 then
      IntegerAttr(IntData(stride.data), IndexType()): d_memref.LayoutParam
    else
      val v = strides(strideIdx)
      strideIdx += 1
      ValueAttribute(v)
  }
  val refinedOffset: d_memref.LayoutParam = ValueAttribute(offset)
  (
    emitted.toSeq,
    d_memref.dMemrefMemrefType(
      dims,
      ty.elementType.asInstanceOf[TypeAttribute],
      Some(refinedOffset),
      Some(refinedStrides),
    ),
  )

private def refinedArgType(
    ranked: RankedMemrefType,
    dimArgs: Seq[Value[Attribute]],
): d_memref.dMemrefMemrefType =
  d_memref.dMemrefMemrefType(
    dimArgs.map(v => ValueAttribute(v)),
    ranked.elementType.asInstanceOf[TypeAttribute],
  )

private def newFunctionArguments(
    oldArgs: Seq[Value[Attribute]],
): (
    Seq[Value[Attribute]],
    Seq[TypeAttribute],
    mutable.Map[Value[Attribute], Value[Attribute]],
  ) =
  val newArgs = mutable.ArrayBuffer.empty[Value[Attribute]]
  val signatureInputs = mutable.ArrayBuffer.empty[TypeAttribute]
  val mapper = mutable.Map.empty[Value[Attribute], Value[Attribute]]
  oldArgs.foreach { oldArg =>
    oldArg.typ match
      case ranked: RankedMemrefType =>
        val dimArgs = ranked.shape.attrValues.indices.map(_ =>
          BlockArgument(dTensor.dTensorNatType()).asInstanceOf[Value[Attribute]]
        )
        newArgs ++= dimArgs
        signatureInputs ++= Seq.fill(dimArgs.size)(dTensor.dTensorNatType())
        val memArg =
          BlockArgument(refinedArgType(ranked, dimArgs)).asInstanceOf[Value[Attribute]]
        newArgs += memArg
        signatureInputs += ranked
        mapper(oldArg) = memArg
      case other =>
        val newArg = BlockArgument(other).asInstanceOf[Value[Attribute]]
        newArgs += newArg
        signatureInputs += other.asInstanceOf[TypeAttribute]
        mapper(oldArg) = newArg
  }
  (newArgs.toSeq, signatureInputs.toSeq, mapper)

private def materializeDimOperands(
    mem: Value[Attribute],
    emitted: mutable.ArrayBuffer[Operation],
): Seq[Operand[Attribute]] =
  mem.typ match
    case ranked: RankedMemrefType =>
      ranked.shape.attrValues.indices.map { axis =>
        val axisConst = arith.Constant(idxAttr(axis), Result(IndexType()))
        val dim = memref.Dim(
          mem.asInstanceOf[Operand[MemrefType]],
          axisConst.result.asInstanceOf[Operand[IndexType]],
          Result(IndexType()),
        )
        val nat = dTensor.IndexToNat(dim.result.asInstanceOf[Operand[IndexType]], Result(dTensor.dTensorNatType()))
        emitted += axisConst
        emitted += dim
        emitted += nat
        nat.res.asInstanceOf[Operand[Attribute]]
      }
    case ranked: d_memref.dMemrefMemrefType =>
      ranked.params.map { param =>
        param.getVal().typ match
          case _: dTensor.dTensorNatType =>
            param.getVal().asInstanceOf[Operand[Attribute]]
          case _: IndexType =>
            val nat = dTensor.IndexToNat(
              param.getVal().asInstanceOf[Operand[IndexType]],
              Result(dTensor.dTensorNatType()),
            )
            emitted += nat
            nat.res.asInstanceOf[Operand[Attribute]]
          case ValueRefType(ref) =>
            ref.getVal().asInstanceOf[Operand[Attribute]]
      }
    case _ => Seq.empty

private def lowerCall(
    op: func.Call,
    valueMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Seq[Operation] =
  val emitted = mutable.ArrayBuffer.empty[Operation]
  val operands = mutable.ArrayBuffer.empty[Operand[Attribute]]
  op._operands.foreach { oldOperand =>
    val remapped = remapValue(oldOperand, valueMapper)
    remapped.typ match
      case _: RankedMemrefType | _: d_memref.dMemrefMemrefType =>
        operands ++= materializeDimOperands(remapped, emitted)
        operands += remapped.asInstanceOf[Operand[Attribute]]
      case _ =>
        operands += remapped.asInstanceOf[Operand[Attribute]]
  }
  val call = func.Call(
    op.callee,
    operands.toSeq,
    op._results.map(r => Result(r.typ.asInstanceOf[Attribute])),
  )
  emitted += call
  emitted.toSeq

private def lowerRegion(
    region: Region,
    outerMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Region =
  val oldBlock = region.blocks.head
  Region(
    Block(oldBlock.arguments.map(_.typ), newArgs =>
      val localMapper = mutable.Map.from(outerMapper)
      localMapper.addAll(oldBlock.arguments.zip(newArgs))
      lowerOps(oldBlock.operations, localMapper)
    )
  )

private def lowerOps(
    ops: Iterable[Operation],
    valueMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Seq[Operation] =
  ops.flatMap { op =>
    val lowered: Seq[Operation] = op match
      case op: memref.Alloc =>
        op.memref.typ match
          case ty: RankedMemrefType =>
            val dims = op.dynamicSizes.map(v =>
              remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]
            )
            val (prefix, refinedTy) = refineBaseMemrefType(ty, dims)
            val alloc = d_memref.Alloc(Result(refinedTy))
            prefix :+ alloc
          case _ =>
            Seq(op.deepCopy(using mutable.Map.empty, valueMapper))
      case op: memref.Dealloc =>
        Seq(
          d_memref.Dealloc(
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]]
          )
        )
      case op: memref.Load =>
        Seq(
          d_memref.Load(
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
            op.indices.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]),
            Result(op.result.typ.asInstanceOf[TypeAttribute]),
          )
        )
      case op: memref.Store =>
        Seq(
          d_memref.Store(
            remapValue(op.value, valueMapper).asInstanceOf[Operand[TypeAttribute]],
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
            op.indices.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]),
          )
        )
      case op: memref.ReinterpretCast =>
        op.res.typ match
          case ty: RankedMemrefType if ty.encoding.exists(_.isInstanceOf[StridedLayoutAttr]) =>
            val sizes = op.sizes.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]])
            val offset = remapValue(op.offset, valueMapper).asInstanceOf[Operand[IndexType]]
            val strides =
              op.strides.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]])
            val (prefix, refinedTy) = refineReinterpretType(ty, sizes, offset, strides)
            val refined = d_memref.ReinterpretCast(
              remapValue(op.src, valueMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
              Result(refinedTy),
            )
            prefix :+ refined
          case _ =>
            Seq(op.deepCopy(using mutable.Map.empty, valueMapper))
      case op: affine.For =>
        Seq(
          d_affine.For(
            op.lowerBoundOperands.map(v =>
              remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]
            ),
            op.upperBoundOperands.map(v =>
              remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]
            ),
            op.inits.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[Attribute]]),
            op.res.map(r => Result(r.typ)),
            op.lowerBoundMap,
            op.upperBoundMap,
            op.step,
            lowerRegion(op.body, valueMapper),
          )
        )
      case op: affine.Yield =>
        Seq(
          d_affine.Yield(
            op.arguments.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[Attribute]])
          )
        )
      case op: affine.Load if identityMap(op.map, op.indices.size) =>
        Seq(
          d_memref.Load(
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
            op.indices.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]),
            Result(op.result.typ.asInstanceOf[TypeAttribute]),
          )
        )
      case op: affine.Store if identityMap(op.map, op.indices.size) =>
        Seq(
          d_memref.Store(
            remapValue(op.value, valueMapper).asInstanceOf[Operand[TypeAttribute]],
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
            op.indices.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]),
          )
        )
      case op: func.Call =>
        lowerCall(op, valueMapper)
      case other =>
        Seq(other.deepCopy(using mutable.Map.empty, valueMapper))
    lowered.lastOption.foreach { rewritten =>
      valueMapper.addAll(op.results.zip(rewritten.results))
    }
    lowered
  }.toSeq

private def lowerFunction(funcOp: func.Func): func.Func =
  if funcOp.body.blocks.isEmpty then funcOp
  else
    val oldEntry = funcOp.body.blocks.head
    val (newArgs, signatureInputs, argMapper) = newFunctionArguments(oldEntry.arguments.toSeq)
    val newEntry = Block.fromArguments(
      newArgs,
      _ => lowerOps(oldEntry.operations, mutable.Map.from(argMapper)),
    )
    val newFunctionType = FunctionType(
      signatureInputs,
      funcOp.function_type.outputs,
    )
    val lowered = func.Func(
      funcOp.sym_name,
      newFunctionType,
      funcOp.sym_visibility,
      Region(newEntry),
    )
    lowered.attributes.addAll(funcOp.attributes)
    if lowered.attributes.contains("scair.original_function_type") ||
        lowered.attributes.contains("llvm.emit_c_interface") ||
        lowered.attributes.contains("scair.emit_bare_interface") ||
        lowered.attributes.contains("scair.emit_descriptor_pointer_interface")
    then
      lowered.attributes += ("scair.original_function_type" -> funcOp.function_type)
    lowered

private val LowerFunc = pattern {
  case op: func.Func
      if op.body.blocks.nonEmpty &&
        (op.body.blocks.head.arguments.exists(_.typ.isInstanceOf[RankedMemrefType]) ||
          op.body.blocks.exists(_.operations.exists {
            case _: memref.Alloc | _: memref.Dealloc | _: memref.Load | _: memref.Store |
                _: memref.ReinterpretCast | _: affine.For | _: affine.Load | _: affine.Store |
                _: affine.Yield =>
              true
            case _ => false
          })) =>
    lowerFunction(op)
}

// Refines baseline memref/affine IR into d_memref/d_affine IR.
// Example: `memref.alloc` / `memref.reinterpret_cast` / `affine.for`
//   -> `d_memref.alloc` / `d_memref.reinterpret_cast` / `d_affine.for`,
//      with dynamic dimensions reified as refined type parameters.
final class RefineDynamicLayoutToDMemref(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "refine-dynamic-layout-to-dmemref"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))
