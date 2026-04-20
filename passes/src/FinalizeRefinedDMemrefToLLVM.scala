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

private def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

private def gepInboundsNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("inbounds"), StringData("nuw")))

private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private var cachedIndexConstants: Option[CachedIndexConstants] = None
  private var refinedIndexMaterializer: Option[RefinedIndexMaterializer] = None
  private val requiredRuntimeDecls = scala.collection.mutable.LinkedHashSet.empty[String]

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
      case _: dTensor.dTensorNatType     => llvmIndexType
      case _: IndexType                  => llvmIndexType
      case other                         => other

  private def cloneValueAttr(attr: ValueAttribute): ValueAttribute =
    ValueAttribute(attr.getVal())

  private def cloneDimParam(param: d_memref.DimParam): d_memref.DimParam =
    param match
      case v: ValueAttribute => cloneValueAttr(v)
      case i: IntegerAttr    => i

  private def cloneLayoutParam(param: d_memref.LayoutParam): d_memref.LayoutParam =
    param match
      case v: ValueAttribute => cloneValueAttr(v)
      case i: IntegerAttr    => i

  private def cloneAttr(attr: Attribute): Attribute =
    attr match
      case FunctionType(inputs, outputs) =>
        FunctionType(
          inputs.map(i => cloneAttr(i).asInstanceOf[TypeAttribute]),
          outputs.map(o => cloneAttr(o).asInstanceOf[TypeAttribute]),
        )
      case ValueRefType(ref) =>
        ValueRefType(cloneValueAttr(ref))
      case v: ValueAttribute =>
        cloneValueAttr(v)
      case d_memref.dMemrefMemrefType(params, elem, offset, strides) =>
        d_memref.dMemrefMemrefType(
          params.map(cloneDimParam),
          cloneAttr(elem).asInstanceOf[TypeAttribute],
          offset.map(cloneLayoutParam),
          strides.map(_.map(cloneLayoutParam)),
        )
      case d_memref.dMemrefVectorType(param, elem) =>
        d_memref.dMemrefVectorType(
          cloneDimParam(param),
          cloneAttr(elem).asInstanceOf[TypeAttribute],
        )
      case d_memref.dMemrefMatrixType(rows, cols, elem) =>
        d_memref.dMemrefMatrixType(
          cloneDimParam(rows),
          cloneDimParam(cols),
          cloneAttr(elem).asInstanceOf[TypeAttribute],
        )
      case dTensor.dTensorTensorType(params, elem) =>
        dTensor.dTensorTensorType(
          params.map(cloneValueAttr),
          cloneAttr(elem).asInstanceOf[TypeAttribute],
        )
      case dTensor.dTensorVectorType(param, elem) =>
        dTensor.dTensorVectorType(
          cloneValueAttr(param),
          cloneAttr(elem).asInstanceOf[TypeAttribute],
        )
      case dTensor.dTensorMatrixType(rows, cols, elem) =>
        dTensor.dTensorMatrixType(
          cloneValueAttr(rows),
          cloneValueAttr(cols),
          cloneAttr(elem).asInstanceOf[TypeAttribute],
        )
      case other =>
        other

  private def loweredFunctionType: FunctionType =
    val inputTypes =
      if funcOp.body.blocks.nonEmpty then
        funcOp.body.blocks.head.arguments.map(arg => convertCarrierType(arg.typ).asInstanceOf[TypeAttribute]).toSeq
      else funcOp.function_type.inputs.map(i => convertCarrierType(i).asInstanceOf[TypeAttribute])
    FunctionType(
      inputTypes,
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
          val alias = llvm.Add(
            newArg.asInstanceOf[Operand[IntegerType | IndexType]],
            asLLVMIndex(zero),
            Result(llvmIndexType),
          )
          emit(entry, alias)
          state.valueMap(oldArg) = alias.res
        case _: dTensor.dTensorNatType =>
          state.valueMap(oldArg) = newArg
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
    ty.params.map {
      case d: ValueAttribute => materializeNatOrIndex(d.getVal(), block)
      case IntegerAttr(IntData(v), _: IndexType | _: IntegerType) =>
        constIndex(v, block)
    }

  private def layoutStrides(
      ty: d_memref.dMemrefMemrefType,
      dims: Seq[Value[Attribute]],
      block: Block,
  ): Seq[Value[Attribute]] =
    ty.strides.map(_.map(materializeLayoutParam(_, block))).getOrElse(buildDefaultStrides(dims, block, constCache))

  private def constIndexValue(v: Value[Attribute]): Option[BigInt] =
    v.owner match
      case Some(llvm.Constant(IntegerAttr(IntData(k), _: IntegerType | _: IndexType), _)) => Some(k)
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
            asLLVMIndex(idx),
            asLLVMIndex(stride),
            Result(llvmIndexType),
          )
          emit(block, mul)
          mul.res
    }
    val summands =
      (if constIndexValue(offset).contains(0) then Seq.empty else Seq(offset)) ++ terms
    if summands.isEmpty then constIndex(0, block)
    else summands.reduceLeft { (lhs, rhs) =>
      val add = llvm.Add(
        asLLVMIndex(lhs),
        asLLVMIndex(rhs),
        Result(llvmIndexType),
      )
      emit(block, add)
      add.res
    }

  private def lowerAllocLike(ty: d_memref.dMemrefMemrefType, block: Block): Value[Attribute] =
    requiredRuntimeDecls += mallocRuntimeName
    val dims = layoutDims(ty, block)
    val numElems =
      if dims.isEmpty then constIndex(1, block)
      else dims.tail.foldLeft(dims.head) { (acc, dim) =>
        val mul = llvm.Mul(asLLVMIndex(acc), asLLVMIndex(dim), Result(llvmIndexType))
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

  private def lowerSubview(op: d_memref.Subview, block: Block): Value[Attribute] =
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
      Seq(asLLVMIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      elemTy,
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
      Seq(asLLVMIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      elemTy,
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
    requiredRuntimeDecls += freeRuntimeName
    emit(
      block,
      llvm.Call(SymbolRefAttr(StringData("free")), Seq(asPtr(ptr).asInstanceOf[Operand[Attribute]]), Seq.empty),
    )

  private def lowerCall(call: func.Call, block: Block): func.Call =
    val loweredOperands = call._operands.map { operand =>
      operand.typ match
        case _: dTensor.dTensorNatType =>
          materializeNatOrIndex(operand, block).asInstanceOf[Operand[Attribute]]
        case ValueRefType(_) =>
          materializeNatOrIndex(operand, block).asInstanceOf[Operand[Attribute]]
        case _ =>
          remap(operand).asInstanceOf[Operand[Attribute]]
    }
    func.Call(
      call.callee,
      loweredOperands,
      call._results.map(r => Result(convertCarrierType(r.typ).asInstanceOf[TypeAttribute])),
    )

  def lower(): func.Func =
    val newBlocks = makeLoweredBlocks()
    cachedIndexConstants = Some(CachedIndexConstants(newBlocks.head))
    refinedIndexMaterializer = Some(RefinedIndexMaterializer(remap, constCache))
    seedEntryIndexAliases(newBlocks.head)
    if funcOp.body.blocks.tail.exists(_.operations.exists {
        case llvm.Constant(IntegerAttr(IntData(v), _: IntegerType | _: IndexType), _) => v == 1
        case _                                                       => false
      })
    then constCache.constIndex(1, newBlocks.head)
    funcOp.body.blocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case c: llvm.Constant =>
          c.value match
            case IntegerAttr(IntData(v), _: IntegerType | _: IndexType) if v == 1 =>
              val one = constCache.one(newBlocks.head)
              state.valueMap(c.res) = one
            case _ =>
              val copied =
                c.value match
                  case IntegerAttr(IntData(v), _: IndexType) =>
                    llvm.Constant(llvmIndexAttr(v), Result(llvmIndexType))
                  case _ =>
                    state.deepCopyOp(c).asInstanceOf[llvm.Constant]
              emit(newBlock, copied)
              state.valueMap(c.res) = copied.res
              c.value match
                case IntegerAttr(IntData(v), _: IntegerType | _: IndexType) if newBlock eq newBlocks.head =>
                  constCache.seed(copied.res, v)
                case _ => ()
        case add: llvm.Add =>
          val lowered = llvm.Add(
            remap(add.lhs).asInstanceOf[Operand[IntegerType | IndexType]],
            remap(add.rhs).asInstanceOf[Operand[IntegerType | IndexType]],
            Result(llvmIndexType),
          )
          emit(newBlock, lowered)
          state.valueMap(add.res) = lowered.res
        case mul: llvm.Mul =>
          val lowered = llvm.Mul(
            remap(mul.lhs).asInstanceOf[Operand[IntegerType | IndexType]],
            remap(mul.rhs).asInstanceOf[Operand[IntegerType | IndexType]],
            Result(llvmIndexType),
          )
          emit(newBlock, lowered)
          state.valueMap(mul.res) = lowered.res
        case cmp: llvm.ICmp =>
          val lowered = llvm.ICmp(
            remap(cmp.lhs).asInstanceOf[Operand[IntegerType | IndexType]],
            remap(cmp.rhs).asInstanceOf[Operand[IntegerType | IndexType]],
            Result(I1),
            cmp.predicate,
          )
          emit(newBlock, lowered)
          state.valueMap(cmp.res) = lowered.res
        case gep: llvm.GetElementPtr =>
          val lowered = llvm.GetElementPtr(
            remap(gep.base).asInstanceOf[Operand[llvm.Ptr]],
            gep.dynamicIndices.map(v => remap(v).asInstanceOf[Operand[IntegerType | IndexType]]),
            Result(llvm.Ptr()),
            gep.rawConstantIndices,
            gep.elem_type,
          )
          emit(newBlock, lowered)
          state.valueMap(gep.res) = lowered.res
        case ptrtoint: llvm.PtrToInt =>
          val lowered = llvm.PtrToInt(
            remap(ptrtoint.in).asInstanceOf[Operand[llvm.Ptr]],
            Result(llvmIndexType),
          )
          emit(newBlock, lowered)
          state.valueMap(ptrtoint.out) = lowered.out
        case op: dTensor.NatConst =>
          val lowered = constIndex(op.value.value.value, newBlock)
          state.valueMap(op.res) = lowered
        case op: dTensor.IndexToNat =>
          state.valueMap(op.res) = materializeNatOrIndex(op.index, newBlock)
        case op: dTensor.ShapeToIndex =>
          state.valueMap(op.res) = materializeNatOrIndex(op.nat, newBlock)
        case op: dTensor.NatAdd =>
          val lowered = llvm.Add(
            asLLVMIndex(materializeNatOrIndex(op.lhs, newBlock)),
            asLLVMIndex(materializeNatOrIndex(op.rhs, newBlock)),
            Result(llvmIndexType),
          )
          emit(newBlock, lowered)
          state.valueMap(op.res) = lowered.res
        case op: dTensor.NatMul =>
          val lowered = llvm.Mul(
            asLLVMIndex(materializeNatOrIndex(op.lhs, newBlock)),
            asLLVMIndex(materializeNatOrIndex(op.rhs, newBlock)),
            Result(llvmIndexType),
          )
          emit(newBlock, lowered)
          state.valueMap(op.res) = lowered.res
        case op: d_memref.Alloc      => state.valueMap(op.res) = lowerAlloc(op, newBlock)
        case op: d_memref.ReinterpretCast =>
          state.valueMap(op.res) = lowerReinterpret(op, newBlock)
        case op: d_memref.Subview =>
          state.valueMap(op.res) = lowerSubview(op, newBlock)
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
        case call: func.Call =>
          val lowered = lowerCall(call, newBlock)
          emit(newBlock, lowered)
          state.valueMap.addAll(call.results.zip(lowered.results))
        case ret: func.Return =>
          emit(newBlock, llvm.Return(ret._operands.map(v => remap(v).asInstanceOf[Operand[Attribute]])))
        case other =>
          val copied = state.deepCopyOp(other)
          emit(newBlock, copied)
          state.valueMap.addAll(other.results.zip(copied.results))
      }
    }
    val lowered = func.Func(funcOp.sym_name, loweredFunctionType, funcOp.sym_visibility, Region(newBlocks))
    lowered.attributes.addAll(funcOp.attributes)
    if !lowered.attributes.contains("scair.original_function_type") &&
        (lowered.attributes.contains("llvm.emit_c_interface") ||
          lowered.attributes.contains("scair.emit_bare_interface") ||
          lowered.attributes.contains("scair.emit_descriptor_pointer_interface"))
    then
      val originalType = cloneAttr(funcOp.function_type).asInstanceOf[FunctionType]
      val argTypeMap = scala.collection.mutable.Map.empty[Value[Attribute], Value[Attribute]]
      funcOp.body.blocks.headOption.foreach { oldEntry =>
        oldEntry.arguments.zip(newBlocks.head.arguments).foreach { case (oldArg, newArg) =>
          argTypeMap(oldArg) = newArg
        }
      }
      AttributeWalker.remapTypeUsesInPlace(originalType)(using argTypeMap)
      lowered.attributes += ("scair.original_function_type" -> originalType)
    lowered

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: d_memref.Alloc | _: d_memref.ReinterpretCast | _: d_memref.Subview | _: d_memref.ExtractStridedMetadata |
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
