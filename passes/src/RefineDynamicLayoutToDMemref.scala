package scair.passes.refine_dynamic_layout_to_d_memref

import scair.MLContext
import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.dialects.func
import scair.dialects.memref
import scair.dialects.scf
import scair.ir.*
import scair.transformations.*

import scala.collection.mutable

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

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
      case (AffineDimExpr(name), i) =>
        map.affineMap.dimensions.indexOf(name) == i
      case _ => false
    }

private def refineBaseMemrefType(
    ty: RankedMemrefType,
    dynamicDims: Seq[Operand[IndexType]],
): (Seq[Operation], d_memref.DMemrefMemrefType) =
  var dynIdx = 0
  val dims: Seq[d_memref.DimParam] = ty.shape.attrValues.map { dim =>
    if dim.data >= 0 then
      IntegerAttr(IntData(dim.data), IndexType())
    else
      val attr = ValueAttribute(dynamicDims(dynIdx))
      dynIdx += 1
      attr
  }
  (
    Seq.empty,
    d_memref.DMemrefMemrefType(dims, ty.elementType.asInstanceOf[TypeAttribute]),
  )

private def refineReinterpretType(
    ty: RankedMemrefType,
    sizes: Seq[Operand[IndexType]],
    offset: Operand[IndexType],
    strides: Seq[Operand[IndexType]],
): (Seq[Operation], d_memref.DMemrefMemrefType) =
  val emitted = mutable.ArrayBuffer.empty[Operation]
  var sizeIdx = 0
  val dims: Seq[d_memref.DimParam] = ty.shape.attrValues.map { dim =>
    if dim.data >= 0 then
      IntegerAttr(IntData(dim.data), IndexType())
    else
      val attr = ValueAttribute(sizes(sizeIdx))
      sizeIdx += 1
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
    d_memref.DMemrefMemrefType(
      dims,
      ty.elementType.asInstanceOf[TypeAttribute],
      Some(refinedOffset),
      Some(refinedStrides),
    ),
  )

private def refinedArgType(
    ranked: RankedMemrefType,
    dimArgs: Seq[Value[Attribute]],
): d_memref.DMemrefMemrefType =
  d_memref.DMemrefMemrefType(
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
          BlockArgument(IndexType()).asInstanceOf[Value[Attribute]]
        )
        newArgs ++= dimArgs
        signatureInputs ++= Seq.fill(dimArgs.size)(IndexType())
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
        emitted += axisConst
        emitted += dim
        dim.result.asInstanceOf[Operand[Attribute]]
      }
    case ranked: d_memref.DMemrefMemrefType =>
      ranked.params.map { param =>
        param match
          case p: ValueAttribute =>
            p.getVal().typ match
              case _: IndexType =>
                p.getVal().asInstanceOf[Operand[Attribute]]
              case ValueRefType(ref) =>
                ref.getVal().asInstanceOf[Operand[Attribute]]
              case other =>
                throw new IllegalArgumentException(
                  s"expected index d_memref dimension, got ${d_memref.DMemrefTypeUtil.renderAttr(other)}"
                )
          case IntegerAttr(IntData(v), _) =>
            val c = arith.Constant(idxAttr(v), Result(IndexType()))
            emitted += c
            c.result.asInstanceOf[Operand[Attribute]]
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
      case _: RankedMemrefType | _: d_memref.DMemrefMemrefType =>
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
  val localMapper = mutable.Map.from(outerMapper)
  val block =
    Block.cloneArgumentTypes(oldBlock.arguments, Seq.empty)(using localMapper)
  block.addOps(lowerOps(oldBlock.operations, localMapper))
  Region(block)

private def lowerRegionWithArgTypes(
    region: Region,
    argTypes: Seq[TypeAttribute],
    outerMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Region =
  val oldBlock = region.blocks.head
  Region(
    Block(argTypes, newArgs =>
      val localMapper = mutable.Map.from(outerMapper)
      localMapper.addAll(oldBlock.arguments.zip(newArgs))
      lowerOps(oldBlock.operations, localMapper)
    )
  )

private def scfYieldTypes(region: Region): Seq[TypeAttribute] =
  region.blocks.head.operations.lastOption match
    case Some(y: scf.YieldOp) =>
      y.resultss.map(_.typ.asInstanceOf[TypeAttribute]).toSeq
    case _ =>
      Seq.empty

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
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.DMemrefMemrefType]]
          )
        )
      case op: memref.Load =>
        Seq(
          d_memref.Load(
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
            op.indices.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]),
            Result(op.result.typ.asInstanceOf[TypeAttribute]),
          )
        )
      case op: memref.Store =>
        Seq(
          d_memref.Store(
            remapValue(op.value, valueMapper).asInstanceOf[Operand[TypeAttribute]],
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
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
              remapValue(op.src, valueMapper).asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
              Result(refinedTy),
            )
            prefix :+ refined
          case _ =>
            Seq(op.deepCopy(using mutable.Map.empty, valueMapper))
      case op: affine.For =>
        val loweredInits =
          op.inits.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[Attribute]])
        val bodyArgTypes =
          Seq(IndexType().asInstanceOf[TypeAttribute]) ++ loweredInits.map(_.typ.asInstanceOf[TypeAttribute])
        val loweredBody = lowerRegionWithArgTypes(op.body, bodyArgTypes, valueMapper)
        if !loweredBody.blocks.head.operations.lastOption.exists(_.isInstanceOf[d_affine.Yield]) then
          loweredBody.blocks.head.addOp(d_affine.Yield(Seq.empty))
        Seq(
          d_affine.For(
            op.lowerBoundOperands.map(v =>
              remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]
            ),
            op.upperBoundOperands.map(v =>
              remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]
            ),
            Seq.empty,
            loweredInits,
            loweredInits.map(v => Result(v.typ.asInstanceOf[TypeAttribute])),
            op.lowerBoundMap,
            op.upperBoundMap,
            op.step,
            loweredBody,
          )
        )
      case op: scf.IfOp =>
        val thenRegion = lowerRegion(op.thenRegion, valueMapper)
        val elseRegion = lowerRegion(op.elseRegion, valueMapper)
        Seq(
          scf.IfOp(
            remapValue(op.condition, valueMapper).asInstanceOf[Operand[IntegerType]],
            thenRegion,
            elseRegion,
            scfYieldTypes(thenRegion).map(Result(_)),
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
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
            op.indices.map(v => remapValue(v, valueMapper).asInstanceOf[Operand[IndexType]]),
            Result(op.result.typ.asInstanceOf[TypeAttribute]),
          )
        )
      case op: affine.Store if identityMap(op.map, op.indices.size) =>
        Seq(
          d_memref.Store(
            remapValue(op.value, valueMapper).asInstanceOf[Operand[TypeAttribute]],
            remapValue(op.memref, valueMapper).asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
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

private def scfIfNeedsRefinement(op: scf.IfOp): Boolean =
  op.results.exists(_.typ.isInstanceOf[RankedMemrefType]) ||
    op.thenRegion.blocks.exists(_.operations.exists {
      case _: memref.Load | _: memref.Store | _: memref.ReinterpretCast |
          _: affine.For | _: affine.Load | _: affine.Store | _: affine.Yield =>
        true
      case _ => false
    }) ||
    op.elseRegion.blocks.exists(_.operations.exists {
      case _: memref.Load | _: memref.Store | _: memref.ReinterpretCast |
          _: affine.For | _: affine.Load | _: affine.Store | _: affine.Yield =>
        true
      case _ => false
    })

private val LowerFunc = pattern {
  case op: func.Func
      if op.body.blocks.nonEmpty &&
        (op.body.blocks.head.arguments.exists(_.typ.isInstanceOf[RankedMemrefType]) ||
          op.body.blocks.exists(_.operations.exists {
            case _: memref.Alloc | _: memref.Dealloc | _: memref.Load | _: memref.Store |
                _: memref.ReinterpretCast | _: affine.For | _: affine.Load | _: affine.Store |
                _: affine.Yield =>
              true
            case ifOp: scf.IfOp =>
              scfIfNeedsRefinement(ifOp)
            case _ => false
          })) =>
    lowerFunction(op)
}

// Refines baseline memref/affine IR into d_memref/d_affine IR.
// Example: `memref.alloc` / `memref.reinterpret_cast` / `affine.for`
//   -> `d_memref.alloc` / `d_memref.reinterpret_cast` / `d_affine.for`,
//      with dynamic dimensions reified as refined type parameters.
final class RefineDynamicLayoutToDMemref(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "refine-dynamic-layout-to-d-memref"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))
