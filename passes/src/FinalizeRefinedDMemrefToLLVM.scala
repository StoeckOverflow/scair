package scair.passes.finalize_refined_dmemref_to_llvm

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_memref
import scair.dialects.func
import scair.dialects.llvm
import scair.ir.*
import scair.passes.lowering_helpers.FunctionLoweringState
import scair.passes.llvm_helpers.*
import scair.transformations.*
import scair.transformations.patterns.*

private def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

private def gepInboundsNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("inbounds"), StringData("nuw")))

private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private var cachedIndexConstants: Option[CachedIndexConstants] = None
  private var refinedIndexMaterializer: Option[RefinedIndexMaterializer] = None

  private def remap(v: Value[Attribute]): Value[Attribute] =
    state.remap(v)

  private def emit(block: Block, op: Operation): Unit =
    block.addOp(op)

  private def constCache: CachedIndexConstants =
    cachedIndexConstants.get

  private def indexMaterializer: RefinedIndexMaterializer =
    refinedIndexMaterializer.get

  private def convertCarrierType(attr: Attribute): Attribute =
    attr match
      case _: d_memref.dMemrefMemrefType => llvm.Ptr()
      case other                         => other

  private def loweredFunctionType: FunctionType =
    FunctionType(
      funcOp.function_type.inputs.map(i => convertCarrierType(i).asInstanceOf[TypeAttribute]),
      funcOp.function_type.outputs.map(o => convertCarrierType(o).asInstanceOf[TypeAttribute]),
    )

  private def makeLoweredBlocks(): Seq[Block] =
    funcOp.body.blocks.map { oldBlock =>
      val nb = Block(oldBlock.arguments.map(arg => convertCarrierType(arg.typ)), Seq.empty)
      state.blockMap(oldBlock) = nb
      state.valueMap.addAll(oldBlock.arguments.zip(nb.arguments))
      nb
    }

  private def seedEntryIndexAliases(entry: Block): Unit =
    funcOp.body.blocks.head.arguments.zip(entry.arguments).foreach { case (oldArg, newArg) =>
      oldArg.typ match
        case _: IndexType =>
          val zero = constIndex(0, entry)
          val alias = llvm.Add(newArg.asInstanceOf[Operand[IndexType]], asIndex(zero), Result(IndexType()))
          emit(entry, alias)
          state.valueMap(oldArg) = alias.res
        case _ => ()
    }

  private def constIndex(v: BigInt, block: Block): Value[Attribute] =
    constCache.constIndex(v, block)

  private def materializeNatOrIndex(v: Value[Attribute], block: Block): Value[Attribute] =
    indexMaterializer.materializeNatOrIndex(v, block)

  private def materializeLayoutParam(param: d_memref.LayoutParam, block: Block): Value[Attribute] =
    indexMaterializer.materializeLayoutParam(param, block)

  private def layoutOffset(ty: d_memref.dMemrefMemrefType, block: Block): Value[Attribute] =
    ty.offset.map(materializeLayoutParam(_, block)).getOrElse(constIndex(0, block))

  private def layoutDims(ty: d_memref.dMemrefMemrefType, block: Block): Seq[Value[Attribute]] =
    ty.params.map(d => materializeNatOrIndex(d.getVal(), block))

  private def layoutStrides(
      ty: d_memref.dMemrefMemrefType,
      dims: Seq[Value[Attribute]],
      block: Block,
  ): Seq[Value[Attribute]] =
    ty.strides.map(_.map(materializeLayoutParam(_, block))).getOrElse(buildDefaultStrides(dims, block, constCache))

  private def constIndexValue(v: Value[Attribute]): Option[BigInt] =
    v.owner match
      case Some(llvm.Constant(IntegerAttr(IntData(k), _: IndexType), _)) => Some(k)
      case _                                                              => None

  private def hasZeroOffset(ty: d_memref.dMemrefMemrefType): Boolean =
    ty.offset match
      case Some(IntegerAttr(IntData(v), _)) => v == 0
      case _                                => false

  private def computeLinearIndex(
      ty: d_memref.dMemrefMemrefType,
      idxs: Seq[Value[Attribute]],
      block: Block,
      useFlags: Boolean,
  ): Value[Attribute] =
    val dims = layoutDims(ty, block)
    val offset = layoutOffset(ty, block)
    val strides = layoutStrides(ty, dims, block)
    val terms = idxs.zip(strides).map { case (idx, stride) =>
      constIndexValue(stride) match
        case Some(1) => idx
        case _ =>
          val mul = llvm.Mul(
            asIndex(idx),
            asIndex(stride),
            Result(IndexType()),
            if useFlags then Some(overflowNSWNuw) else None,
          )
          emit(block, mul)
          mul.res
    }
    val summands =
      (if constIndexValue(offset).contains(0) then Seq.empty else Seq(offset)) ++ terms
    if summands.isEmpty then constIndex(0, block)
    else summands.reduceLeft { (lhs, rhs) =>
      val add = llvm.Add(
        asIndex(lhs),
        asIndex(rhs),
        Result(IndexType()),
        if useFlags then Some(overflowNSWNuw) else None,
      )
      emit(block, add)
      add.res
    }

  private def lowerAllocLike(ty: d_memref.dMemrefMemrefType, block: Block): Value[Attribute] =
    val dims = layoutDims(ty, block)
    val numElems =
      if dims.isEmpty then constIndex(1, block)
      else dims.tail.foldLeft(dims.head) { (acc, dim) =>
        val mul = llvm.Mul(asIndex(acc), asIndex(dim), Result(IndexType()))
        emit(block, mul)
        mul.res
      }
    val sizeBytes = computeAllocationSizeBytes(numElems, ty.elem, block)
    val malloc = llvm.Call(
      SymbolRefAttr(StringData("malloc")),
      Seq(sizeBytes.asInstanceOf[Operand[Attribute]]),
      Seq(Result(llvm.Ptr())),
    )
    emit(block, malloc)
    malloc.resultss.head

  private def lowerAlloc(op: d_memref.Alloc, block: Block): Value[Attribute] =
    lowerAllocLike(op.res.typ, block)

  private def lowerReinterpret(op: d_memref.ReinterpretCast, block: Block): Value[Attribute] =
    remap(op.src)

  private def lowerLoadFromLinearized(
      base: Value[Attribute],
      elemTy: Attribute,
      linear: Value[Attribute],
      resultTy: Attribute,
      block: Block,
      useFlags: Boolean,
  ): Value[Attribute] =
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      elemTy,
      if useFlags then Some(gepInboundsNuw) else None,
    )
    emit(block, gep)
    val load = llvm.Load(asPtr(gep.res), Result(resultTy))
    emit(block, load)
    load.res

  private def lowerStoreToLinearized(
      value: Value[Attribute],
      base: Value[Attribute],
      elemTy: Attribute,
      linear: Value[Attribute],
      block: Block,
      useFlags: Boolean,
  ): Unit =
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      elemTy,
      if useFlags then Some(gepInboundsNuw) else None,
    )
    emit(block, gep)
    emit(block, llvm.Store(value.asInstanceOf[Operand[Attribute]], asPtr(gep.res)))

  private def lowerLoad(op: d_memref.Load, block: Block): Value[Attribute] =
    val idxs = op.indices.map(materializeNatOrIndex(_, block))
    val flagged = hasZeroOffset(op.memref.typ) && idxs.size == 2
    val linear = computeLinearIndex(op.memref.typ, idxs, block, flagged)
    lowerLoadFromLinearized(remap(op.memref), op.memref.typ.elem, linear, op.res.typ, block, flagged)

  private def lowerStore(op: d_memref.Store, block: Block): Unit =
    val idxs = op.indices.map(materializeNatOrIndex(_, block))
    val flagged = hasZeroOffset(op.memref.typ) && idxs.size == 2
    val linear = computeLinearIndex(op.memref.typ, idxs, block, flagged)
    lowerStoreToLinearized(remap(op.value), remap(op.memref), op.memref.typ.elem, linear, block, flagged)

  private def lowerExtractStridedMetadata(op: d_memref.ExtractStridedMetadata, block: Block): Seq[Value[Attribute]] =
    val srcTy = op.source.typ
    val dims = layoutDims(srcTy, block)
    val offset = layoutOffset(srcTy, block)
    val strides = layoutStrides(srcTy, dims, block)
    Seq(remap(op.source), offset) ++ dims ++ strides

  private def lowerDealloc(ptr: Value[Attribute], block: Block): Unit =
    emit(
      block,
      llvm.Call(SymbolRefAttr(StringData("free")), Seq(asPtr(ptr).asInstanceOf[Operand[Attribute]]), Seq.empty),
    )

  def lower(): func.Func =
    val newBlocks = makeLoweredBlocks()
    cachedIndexConstants = Some(CachedIndexConstants(newBlocks.head))
    refinedIndexMaterializer = Some(RefinedIndexMaterializer(remap, constCache))
    seedEntryIndexAliases(newBlocks.head)
    if funcOp.body.blocks.tail.exists(_.operations.exists {
        case llvm.Constant(IntegerAttr(IntData(v), _: IndexType), _) => v == 1
        case _                                                       => false
      })
    then constCache.constIndex(1, newBlocks.head)
    funcOp.body.blocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case c: llvm.Constant =>
          c.value match
            case IntegerAttr(IntData(v), _: IndexType) if v == 1 =>
              val one = constCache.one(newBlocks.head)
              state.valueMap(c.res) = one
            case _ =>
              val copied = state.deepCopyOp(c).asInstanceOf[llvm.Constant]
              emit(newBlock, copied)
              state.valueMap(c.res) = copied.res
              c.value match
                case IntegerAttr(IntData(v), _: IndexType) => constCache.seed(copied.res, v)
                case _                                               => ()
        case _: dTensor.NatConst     => ()
        case _: dTensor.IndexToNat   => ()
        case _: dTensor.ShapeToIndex => ()
        case op: d_memref.Alloc      => state.valueMap(op.res) = lowerAlloc(op, newBlock)
        case op: d_memref.ReinterpretCast =>
          state.valueMap(op.res) = lowerReinterpret(op, newBlock)
        case op: d_memref.ExtractStridedMetadata =>
          state.valueMap.addAll(op.results.zip(lowerExtractStridedMetadata(op, newBlock)))
        case op: d_memref.Load =>
          state.valueMap(op.res) = lowerLoad(op, newBlock)
        case op: d_memref.Store =>
          lowerStore(op, newBlock)
        case op: d_memref.Cast =>
          state.valueMap(op.res) = remap(op.src)
        case op: d_memref.Dealloc =>
          lowerDealloc(remap(op.memref), newBlock)
        case ret: func.Return =>
          emit(newBlock, llvm.Return(ret._operands.map(v => remap(v).asInstanceOf[Operand[Attribute]])))
        case other =>
          val copied = state.deepCopyOp(other)
          emit(newBlock, copied)
          state.valueMap.addAll(other.results.zip(copied.results))
      }
    }
    func.Func(funcOp.sym_name, loweredFunctionType, funcOp.sym_visibility, Region(newBlocks))

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: d_memref.Alloc | _: d_memref.ReinterpretCast | _: d_memref.ExtractStridedMetadata |
            _: d_memref.Load | _: d_memref.Store | _: d_memref.Cast |
            _: d_memref.Dealloc | _: func.Return =>
          true
        case _ => false
      }) =>
    Builder(op).lower()
}

final class FinalizeRefinedDMemrefToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "finalize-refined-dmemref-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))
