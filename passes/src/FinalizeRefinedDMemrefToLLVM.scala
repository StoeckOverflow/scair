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

import scala.collection.mutable

private def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

private def gepInboundsNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("inbounds"), StringData("nuw")))

// Finalization is implemented as a whole-function rebuild because refined
// memref lowering simultaneously replaces refined ops, materializes layout
// parameters, and preserves the surrounding LLVM CFG.
private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private var cachedOne: Option[Value[Attribute]] = None
  private var cachedZero: Option[Value[Attribute]] = None

  private def remap(v: Value[Attribute]): Value[Attribute] =
    state.remap(v)

  private def emit(block: Block, op: Operation): Unit =
    block.addOp(op)

  private def constNat(v: Value[Attribute]): Option[BigInt] =
    v.owner match
      case Some(dTensor.NatConst(IntegerAttr(IntData(k), _), _)) => Some(k)
      case _                                                     => None

  private def constIndex(v: BigInt, block: Block): Value[Attribute] =
    if v == 0 && cachedZero.nonEmpty then cachedZero.get
    else if v == 1 && cachedOne.nonEmpty then cachedOne.get
    else
      val c = llvm.Constant(idxAttr(v), Result(IndexType()))
      emit(block, c)
      if v == 0 then cachedZero = Some(c.res)
      if v == 1 then cachedOne = Some(c.res)
      c.res

  // Refined types may encode layout through nat-valued SSA parameters. This
  // helper collapses those representations to the index SSA form expected by
  // the final LLVM address arithmetic.
  private def materializeNatOrIndex(v: Value[Attribute], block: Block): Value[Attribute] =
    remap(v) match
      case existing if existing.owner.exists {
            case op: Operation => op.name.startsWith("llvm.")
            case _             => false
          } =>
        existing
      case other =>
        constNat(other).map(k => constIndex(k, block)).orElse {
          other.owner.collect {
            case dTensor.IndexToNat(idx, _) =>
              materializeNatOrIndex(idx, block)
            case dTensor.ShapeToIndex(nat, _) =>
              constNat(nat).map(k => constIndex(k, block)).getOrElse(other)
          }
        }.getOrElse(other)

  private def materializeLayoutParam(
      param: d_memref.LayoutParam,
      block: Block,
  ): Value[Attribute] =
    param match
      case i: IntegerAttr =>
        constIndex(i.value.value, block)
      case v: ValueAttribute =>
        materializeNatOrIndex(v.getVal(), block)

  private def descriptor(
      desc: Value[Attribute],
      rank: Int,
      block: Block,
  ): RankedMemrefDescriptorHelper =
    RankedMemrefDescriptorHelper(desc, rank, block)

  // When the refined type omits explicit strides, finalize reconstructs the
  // canonical row-major defaults from the refined dimension values.
  private def buildDefaultStrides(
      dims: Seq[Value[Attribute]],
      block: Block,
  ): Seq[Value[Attribute]] =
    if dims.isEmpty then Seq.empty
    else
      val one = constIndex(1, block)
      val rev = mutable.ArrayBuffer[Value[Attribute]](one)
      dims.reverse.drop(1).foreach { dim =>
        val mul = llvm.Mul(asIndex(dim), asIndex(rev.last), Result(IndexType()))
        emit(block, mul)
        rev += mul.res
      }
      rev.reverse.toSeq

  private def lowerAllocLike(
      ty: d_memref.dMemrefMemrefType,
      block: Block,
  ): Value[Attribute] =
    val dims = ty.params.map(d => materializeNatOrIndex(d.getVal(), block))
    val offset = ty.offset.map(materializeLayoutParam(_, block)).getOrElse(constIndex(0, block))
    val strides = ty.strides.map(_.map(materializeLayoutParam(_, block))).getOrElse(buildDefaultStrides(dims, block))
    val numElems =
      if dims.isEmpty then constIndex(1, block)
      else dims.tail.foldLeft(dims.head) { (acc, dim) =>
        val mul = llvm.Mul(asIndex(acc), asIndex(dim), Result(IndexType()))
        emit(block, mul)
        mul.res
      }
    val nullPtr = llvm.Zero(Result(llvm.Ptr()))
    emit(block, nullPtr)
    val sizePtr = llvm.GetElementPtr(
      asPtr(nullPtr.res),
      Seq(asIndex(numElems)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      ty.elem,
    )
    emit(block, sizePtr)
    val sizeBytes = llvm.PtrToInt(asPtr(sizePtr.res), Result(IndexType()))
    emit(block, sizeBytes)
    val malloc = llvm.Call(
      SymbolRefAttr(StringData("malloc")),
      Seq(sizeBytes.out.asInstanceOf[Operand[Attribute]]),
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

  private def lowerAlloc(op: d_memref.Alloc, block: Block): Value[Attribute] =
    lowerAllocLike(op.res.typ, block)

  private def lowerReinterpret(
      src: Value[Attribute],
      targetTy: d_memref.dMemrefMemrefType,
      offset: Value[Attribute],
      sizes: Seq[Value[Attribute]],
      strides: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    val srcDesc = descriptor(src, targetTy.params.size, block)
    RankedMemrefDescriptorHelper.build(
      srcDesc.allocatedPtr(),
      srcDesc.alignedPtr(),
      offset,
      sizes,
      strides,
      block,
    )

  private def lowerReinterpret(
      op: d_memref.ReinterpretCast,
      block: Block,
  ): Value[Attribute] =
    lowerReinterpret(
      remap(op.src),
      op.res.typ,
      materializeNatOrIndex(op.offset, block),
      op.sizes.map(materializeNatOrIndex(_, block)),
      op.strides.map(materializeNatOrIndex(_, block)),
      block,
    )

  // Direct refined loads use the current descriptor value, but their indices
  // have already been normalized to explicit SSA values by earlier passes.
  private def lowerLoad(
      desc: Value[Attribute],
      ty: d_memref.dMemrefMemrefType,
      idxs: Seq[Value[Attribute]],
      resultTy: Attribute,
      block: Block,
  ): Value[Attribute] =
    val memrefDesc = descriptor(desc, idxs.size, block)
    val base = memrefDesc.alignedPtr()
    val flagged = ty.offset.exists {
      case IntegerAttr(IntData(v), _) => v == 0
      case _                          => false
    } && idxs.size == 2
    val terms = idxs.zipWithIndex.map { case (idx, axis) =>
      val stride = memrefDesc.stride(axis)
      val mul = llvm.Mul(
        asIndex(idx),
        asIndex(stride),
        Result(IndexType()),
        if flagged then Some(overflowNSWNuw) else None,
      )
      emit(block, mul)
      mul.res
    }
    val linear =
      if terms.size == 1 then terms.head
      else
        terms.reduce { (l, r) =>
          val add = llvm.Add(
            asIndex(l),
            asIndex(r),
            Result(IndexType()),
            if flagged then Some(overflowNSWNuw) else None,
          )
          emit(block, add)
          add.res
        }
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      ty.elem,
      if flagged then Some(gepInboundsNuw) else None,
    )
    emit(block, gep)
    val load = llvm.Load(asPtr(gep.res), Result(resultTy))
    emit(block, load)
    load.res

  private def lowerLoad(op: d_memref.Load, block: Block): Value[Attribute] =
    lowerLoad(
      remap(op.memref),
      op.memref.typ,
      op.indices.map(materializeNatOrIndex(_, block)),
      op.res.typ,
      block,
    )

  // Linearized loads consume the already-normalized linear index directly.
  private def lowerLinearizedLoad(
      op: d_memref.LinearizedLoad,
      block: Block,
  ): Value[Attribute] =
    val ty = op.memref.typ
    val desc = remap(op.memref)
    val linear = materializeNatOrIndex(op.linearIndex, block)
    val base = descriptor(desc, ty.params.size, block).alignedPtr()
    val flagged = ty.offset.exists {
      case IntegerAttr(IntData(v), _) => v == 0
      case _                          => false
    }
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      ty.elem,
      if flagged then Some(gepInboundsNuw) else None,
    )
    emit(block, gep)
    val load = llvm.Load(asPtr(gep.res), Result(op.res.typ))
    emit(block, load)
    load.res

  private def lowerBasePtr(op: d_memref.BasePtr, block: Block): Value[Attribute] =
    descriptor(remap(op.memref), op.memref.typ.params.size, block).alignedPtr()

  // This is the end point of the refined access normalization path: a hoisted
  // base pointer plus a linearized offset become a single GEP+load pair.
  private def lowerLinearizedLoadFromBase(
      op: d_memref.LinearizedLoadFromBase,
      block: Block,
  ): Value[Attribute] =
    val base = remap(op.base)
    val linear = materializeNatOrIndex(op.linearIndex, block)
    val gep = llvm.GetElementPtr(
      asPtr(base),
      Seq(asIndex(linear)),
      Result(llvm.Ptr()),
      dynamicIndexSentinel,
      op.res.typ,
      Some(gepInboundsNuw),
    )
    emit(block, gep)
    val load = llvm.Load(asPtr(gep.res), Result(op.res.typ))
    emit(block, load)
    load.res

  private def lowerExtractStridedMetadata(
      op: d_memref.ExtractStridedMetadata,
      block: Block,
  ): Seq[Value[Attribute]] =
    val src = remap(op.source)
    val srcTy = op.source.typ
    val rank = srcTy.params.size
    val srcDesc = descriptor(src, rank, block)
    def used(v: Value[Attribute]): Boolean =
      v.uses.nonEmpty || v.typeUses.nonEmpty

    // As on the baseline side, the extracted base currently aliases the source
    // descriptor value. This preserves the existing LLVM IR shape.
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
    val rank = desc.typ match
      case llvm.StructType(fields) =>
        fields(3).asInstanceOf[llvm.ArrayType].size.value.toInt
      case _ => 0
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
    val newBlocks = state.makeClonedBlocks()
    funcOp.body.blocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case c: llvm.Constant =>
          // Reuse the entry-block index-1 constant when possible so the refined
          // routes preserve the current stable constant shape in snapshot tests.
          c.value match
            case IntegerAttr(IntData(v), _: IndexType) if v == 1 && !(oldBlock eq funcOp.body.blocks.head) =>
              state.valueMap(c.res) = cachedOne.getOrElse(constIndex(1, newBlocks.head))
            case _ =>
              val copied = state.deepCopyOp(c).asInstanceOf[llvm.Constant]
              emit(newBlock, copied)
              state.valueMap(c.res) = copied.res
              c.value match
                case IntegerAttr(IntData(v), _: IndexType) if v == 1 =>
                  cachedOne = Some(copied.res)
                case IntegerAttr(IntData(v), _: IndexType) if v == 0 =>
                  cachedZero = Some(copied.res)
                case _ => ()
        case _: dTensor.NatConst =>
          ()
        case _: dTensor.IndexToNat =>
          ()
        case _: dTensor.ShapeToIndex =>
          ()
        case op: d_memref.Alloc =>
          state.valueMap(op.res) = lowerAlloc(op, newBlock)
        case op: d_memref.ReinterpretCast =>
          state.valueMap(op.res) = lowerReinterpret(op, newBlock)
        case op: d_memref.ExtractStridedMetadata =>
          state.valueMap.addAll(op.results.zip(lowerExtractStridedMetadata(op, newBlock)))
        case op: d_memref.Load =>
          state.valueMap(op.res) = lowerLoad(op, newBlock)
        case op: d_memref.LinearizedLoad =>
          state.valueMap(op.res) = lowerLinearizedLoad(op, newBlock)
        case op: d_memref.BasePtr =>
          state.valueMap(op.res) = lowerBasePtr(op, newBlock)
        case op: d_memref.LinearizedLoadFromBase =>
          state.valueMap(op.res) = lowerLinearizedLoadFromBase(op, newBlock)
        case op: d_memref.Dealloc =>
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
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(newBlocks))

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: d_memref.Alloc | _: d_memref.ReinterpretCast | _: d_memref.ExtractStridedMetadata |
            _: d_memref.Load | _: d_memref.LinearizedLoad | _: d_memref.BasePtr |
            _: d_memref.LinearizedLoadFromBase | _: d_memref.Dealloc | _: func.Return =>
          true
        case _ => false
      }) =>
    Builder(op).lower()
}

// Finalizes refined d_memref operations to standard LLVM dialect operations.
// Example: `d_memref.alloc` / `d_memref.base_ptr` /
// `d_memref.linearized_load_from_base`
//   -> LLVM descriptor construction, pointer extraction, GEP, and load.
final class FinalizeRefinedDMemrefToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "finalize-refined-dmemref-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))
