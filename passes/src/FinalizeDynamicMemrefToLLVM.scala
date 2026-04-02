package scair.passes.finalize_dynamic_memref_to_llvm

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.func
import scair.dialects.llvm
import scair.dialects.memref
import scair.ir.*
import scair.passes.lowering_helpers.FunctionLoweringState
import scair.passes.llvm_helpers.*
import scair.transformations.*
import scair.transformations.patterns.*

private def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

private def gepInboundsNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("inbounds"), StringData("nuw")))

// Finalization is implemented as a whole-function rebuild because memref
// lowering must replace descriptor-valued operations and preserve SSA uses.
private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private var cachedIndexConstants: Option[CachedIndexConstants] = None

  private def remap(v: Value[Attribute]): Value[Attribute] =
    state.remap(v)

  private def emit(block: Block, op: Operation): Unit =
    block.addOp(op)

  private def constCache: CachedIndexConstants =
    cachedIndexConstants.get

  private def constIndex(v: BigInt, block: Block): Value[Attribute] =
    constCache.constIndex(v, block)

  private def convertCarrierType(attr: Attribute): Attribute =
    attr match
      case ranked: RankedMemrefType =>
        RankedMemrefDescriptorHelper.descriptorType(ranked.shape.attrValues.size)
      case _: IndexType => llvmIndexType
      case other => other

  private def loweredFunctionType: FunctionType =
    val inputTypes =
      if funcOp.body.blocks.nonEmpty then
        funcOp.body.blocks.head.arguments.map(arg => convertCarrierType(arg.typ).asInstanceOf[TypeAttribute]).toSeq
      else funcOp.function_type.inputs.map(i => convertCarrierType(i).asInstanceOf[TypeAttribute])
    FunctionType(
      inputTypes,
      funcOp.function_type.outputs.map(o => convertCarrierType(o).asInstanceOf[TypeAttribute]),
    )

  private def descriptor(
      desc: Value[Attribute],
      rank: Int,
      block: Block,
  ): RankedMemrefDescriptorHelper =
    RankedMemrefDescriptorHelper(desc, rank, block)

  // Baseline memref allocation materializes the standard ranked memref
  // descriptor shape directly in LLVM dialect.
  private def materializeRankedDims(
      ty: RankedMemrefType,
      dynamicSizes: Seq[Value[Attribute]],
      block: Block,
  ): Seq[Value[Attribute]] =
    var dynIdx = 0
    ty.shape.attrValues.map { dim =>
      if dim.data >= 0 then constIndex(dim.data, block)
      else
        val v = remap(dynamicSizes(dynIdx))
        dynIdx += 1
        v
    }

  private def lowerAllocLike(
      ty: RankedMemrefType,
      dynamicSizes: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    val dims = materializeRankedDims(ty, dynamicSizes.map(remap), block)
    val offset = constIndex(0, block)
    val strides = buildDefaultStrides(dims, block, constCache)
    val numElems =
      if dims.isEmpty then constIndex(1, block)
      else dims.tail.foldLeft(dims.head) { (acc, dim) =>
        val mul = llvm.Mul(asLLVMIndex(acc), asLLVMIndex(dim), Result(llvmIndexType))
        emit(block, mul)
        mul.res
      }
    val sizeBytes = computeAllocationSizeBytes(numElems, ty.elementType, block)
    val malloc = llvm.Call(
      SymbolRefAttr(StringData("malloc")),
      Seq(sizeBytes.asInstanceOf[Operand[Attribute]]),
      Seq(Result(llvm.Ptr())),
    )
    emit(block, malloc)
    RankedMemrefDescriptorHelper.build(
      malloc.resultss.head,
      malloc.resultss.head,
      offset,
      dims,
      strides,
      block,
    )

  private def lowerAlloc(op: memref.Alloc, block: Block): Value[Attribute] =
    lowerAllocLike(op.memref.typ.asInstanceOf[RankedMemrefType], op.dynamicSizes, block)

  private def lowerReinterpret(
      src: Value[Attribute],
      targetTy: RankedMemrefType,
      offset: Value[Attribute],
      sizes: Seq[Value[Attribute]],
      strides: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    val srcDesc = descriptor(src, targetTy.shape.attrValues.size, block)
    RankedMemrefDescriptorHelper.build(
      srcDesc.allocatedPtr(),
      srcDesc.alignedPtr(),
      offset,
      sizes,
      strides,
      block,
    )

  private def lowerReinterpret(
      op: memref.ReinterpretCast,
      block: Block,
  ): Value[Attribute] =
    lowerReinterpret(
      remap(op.src),
      op.res.typ.asInstanceOf[RankedMemrefType],
      remap(op.offset),
      op.sizes.map(remap),
      op.strides.map(remap),
      block,
    )

  // Baseline loads still reconstruct the linear address from descriptor strides
  // at finalize time because the baseline route does not expose this arithmetic
  // earlier in the pipeline.
  private def lowerLoad(
      desc: Value[Attribute],
      ty: RankedMemrefType,
      idxs: Seq[Value[Attribute]],
      resultTy: Attribute,
      block: Block,
  ): Value[Attribute] =
    val memrefDesc = descriptor(desc, idxs.size, block)
    val base = memrefDesc.alignedPtr()
    val layout = ty.encoding.collect { case s: StridedLayoutAttr => s }
    val flagged = layout.exists(_.offset.data == 0) && idxs.size == 2
    val terms = idxs.zipWithIndex.map { case (idx, axis) =>
      val stride = memrefDesc.stride(axis)
      val mul = llvm.Mul(
        asLLVMIndex(idx),
        asLLVMIndex(stride),
        Result(llvmIndexType),
      )
      emit(block, mul)
      mul.res
    }
    val linear = terms.reduce { (l, r) =>
      val add = llvm.Add(
        asLLVMIndex(l),
        asLLVMIndex(r),
        Result(llvmIndexType),
      )
      emit(block, add)
      add.res
    }
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asLLVMIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      ty.elementType,
    )
    emit(block, gep)
    val load = llvm.Load(asPtr(gep.res), Result(resultTy))
    emit(block, load)
    load.res

  private def lowerLoad(op: memref.Load, block: Block): Value[Attribute] =
    lowerLoad(
      remap(op.memref),
      op.memref.typ.asInstanceOf[RankedMemrefType],
      op.indices.map(remap),
      op.result.typ,
      block,
    )

  private def lowerStore(
      desc: Value[Attribute],
      ty: RankedMemrefType,
      idxs: Seq[Value[Attribute]],
      value: Value[Attribute],
      block: Block,
  ): Unit =
    val memrefDesc = descriptor(desc, idxs.size, block)
    val base = memrefDesc.alignedPtr()
    val layout = ty.encoding.collect { case s: StridedLayoutAttr => s }
    val flagged = layout.exists(_.offset.data == 0) && idxs.size == 2
    val terms = idxs.zipWithIndex.map { case (idx, axis) =>
      val stride = memrefDesc.stride(axis)
      val mul = llvm.Mul(
        asLLVMIndex(idx),
        asLLVMIndex(stride),
        Result(llvmIndexType),
      )
      emit(block, mul)
      mul.res
    }
    val linear =
      if terms.size == 1 then terms.head
      else
        terms.reduce { (l, r) =>
          val add = llvm.Add(
            asLLVMIndex(l),
            asLLVMIndex(r),
            Result(llvmIndexType),
          )
          emit(block, add)
          add.res
        }
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asLLVMIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      ty.elementType,
    )
    emit(block, gep)
    emit(block, llvm.Store(value.asInstanceOf[Operand[Attribute]], asPtr(gep.res)))

  private def lowerStore(op: memref.Store, block: Block): Unit =
    lowerStore(
      remap(op.memref),
      op.memref.typ.asInstanceOf[RankedMemrefType],
      op.indices.map(remap),
      remap(op.value),
      block,
    )

  private def lowerExtractStridedMetadata(
      op: memref.ExtractStridedMetadata,
      block: Block,
  ): Seq[Value[Attribute]] =
    val src = remap(op.source)
    val srcTy = op.source.typ.asInstanceOf[RankedMemrefType]
    val rank = srcTy.shape.attrValues.size
    val srcDesc = descriptor(src, rank, block)
    def used(v: Value[Attribute]): Boolean =
      v.uses.nonEmpty || v.typeUses.nonEmpty

    // The base result follows the current pragmatic convention used in this
    // codebase: it aliases the source descriptor instead of constructing a new
    // standalone base descriptor value.
    val base = src
    val offset =
      if used(op.results(1)) then srcDesc.offset()
      else constIndex(0, block)
    val sizes = (0 until rank).map { i =>
      if used(op.results(2 + i)) then srcDesc.size(i)
      else constIndex(0, block)
    }
    val strides = (0 until rank).map { i =>
      if used(op.results(2 + rank + i)) then srcDesc.stride(i)
      else constIndex(0, block)
    }
    Seq(base, offset) ++ sizes ++ strides

  private def lowerDealloc(desc: Value[Attribute], block: Block): Unit =
    val rank = RankedMemrefDescriptorHelper.rankOfDescriptorType(desc.typ).getOrElse(0)
    val ptr = descriptor(desc, rank, block).allocatedPtr()
    emit(
      block,
      llvm.Call(
        SymbolRefAttr(StringData("free")),
        Seq(ptr.asInstanceOf[Operand[Attribute]]),
        Seq.empty,
      ),
    )

  def lower(): func.Func =
    val clonedBlocks = funcOp.body.blocks.map { oldBlock =>
      val nb = Block(oldBlock.arguments.map(arg => convertCarrierType(arg.typ)), Seq.empty)
      state.blockMap(oldBlock) = nb
      state.valueMap.addAll(oldBlock.arguments.zip(nb.arguments))
      nb
    }
    cachedIndexConstants = Some(CachedIndexConstants(clonedBlocks.head))
    funcOp.body.blocks.zip(clonedBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case c: llvm.Constant =>
          val copied =
            c.value match
              case IntegerAttr(IntData(v), _: IndexType) =>
                llvm.Constant(llvmIndexAttr(v), Result(llvmIndexType))
              case _ =>
                state.deepCopyOp(c).asInstanceOf[llvm.Constant]
          emit(newBlock, copied)
          state.valueMap(c.res) = copied.res
          c.value match
            case IntegerAttr(IntData(v), _: IndexType | _: IntegerType) =>
              constCache.seed(copied.res, v)
            case _ => ()
        case op: memref.Alloc =>
          state.valueMap(op.memref) = lowerAlloc(op, newBlock)
        case op: memref.ReinterpretCast =>
          state.valueMap(op.res) = lowerReinterpret(op, newBlock)
        case op: memref.ExtractStridedMetadata =>
          state.valueMap.addAll(op.results.zip(lowerExtractStridedMetadata(op, newBlock)))
        case op: memref.Load =>
          state.valueMap(op.result) = lowerLoad(op, newBlock)
        case op: memref.Store =>
          lowerStore(op, newBlock)
        case op: memref.Dealloc =>
          lowerDealloc(remap(op.memref), newBlock)
        case ret: func.Return =>
          emit(
            newBlock,
            llvm.Return(ret._operands.map(v => remap(v).asInstanceOf[Operand[Attribute]])),
          )
        case other =>
          val copied = state.deepCopyOp(other)
          emit(newBlock, copied)
          state.valueMap.addAll(other.results.zip(copied.results))
      }
    }
    val lowered = func.Func(funcOp.sym_name, loweredFunctionType, funcOp.sym_visibility, Region(clonedBlocks))
    lowered.attributes.addAll(funcOp.attributes)
    if !lowered.attributes.contains("scair.original_function_type") &&
        (lowered.attributes.contains("llvm.emit_c_interface") ||
          lowered.attributes.contains("scair.emit_bare_interface") ||
          lowered.attributes.contains("scair.emit_descriptor_pointer_interface"))
    then
      lowered.attributes += ("scair.original_function_type" -> funcOp.function_type)
    lowered

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: memref.Alloc | _: memref.ReinterpretCast | _: memref.ExtractStridedMetadata | _: memref.Load | _: memref.Store | _: memref.Dealloc | _: func.Return =>
          true
        case _ => false
      }) =>
    Builder(op).lower()
}

// Finalizes baseline memref operations to standard LLVM dialect operations.
// Example: `memref.alloc` / `memref.load` / `memref.dealloc`
//   -> LLVM memref descriptor construction, `llvm.getelementptr`, `llvm.load`,
//      and `llvm.call @free`.
final class FinalizeDynamicMemrefToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "finalize-dynamic-memref-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))
