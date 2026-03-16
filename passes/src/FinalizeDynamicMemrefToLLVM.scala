package scair.passes.finalize_dynamic_memref_to_llvm

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.func
import scair.dialects.llvm
import scair.dialects.memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private def i32Attr(v: Int): IntegerAttr =
  IntegerAttr(IntData(v), I32)

private def densePath(indices: Int*): DenseArrayAttr =
  DenseArrayAttr(I32, indices.map(i => i32Attr(i)))

private def dynamicIndexSentinel: DenseArrayAttr =
  DenseArrayAttr(I32, Seq(i32Attr(-2147483648)))

private def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

private def gepInboundsNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("inbounds"), StringData("nuw")))

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asPtr(v: Value[Attribute]): Operand[llvm.Ptr] =
  v.asInstanceOf[Operand[llvm.Ptr]]

private final class Builder(val funcOp: func.Func):
  val blockMap = mutable.Map.empty[Block, Block]
  val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
  private var cachedOne: Option[Value[Attribute]] = None
  private var cachedZero: Option[Value[Attribute]] = None

  private def remap(v: Value[Attribute]): Value[Attribute] = valueMap.getOrElse(v, v)
  private def emit(block: Block, op: Operation): Unit = block.addOp(op)

  private def constIndex(v: BigInt, block: Block): Value[Attribute] =
    if v == 0 && cachedZero.nonEmpty then cachedZero.get
    else if v == 1 && cachedOne.nonEmpty then cachedOne.get
    else
      val c = llvm.Constant(idxAttr(v), Result(IndexType()))
      emit(block, c)
      if v == 0 then cachedZero = Some(c.res)
      if v == 1 then cachedOne = Some(c.res)
      c.res

  private def lowerExtract(agg: Value[Attribute], path: Seq[Int], resTy: Attribute, block: Block): Value[Attribute] =
    val ex = llvm.ExtractValue(agg.asInstanceOf[Operand[Attribute]], densePath(path*), Result(resTy))
    emit(block, ex)
    ex.res

  private def buildDescriptor(
      allocatedPtr: Value[Attribute],
      alignedPtr: Value[Attribute],
      offset: Value[Attribute],
      sizes: Seq[Value[Attribute]],
      strides: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    val descTy = llvm.StructType(
      Seq(
        llvm.Ptr(),
        llvm.Ptr(),
        IndexType(),
        llvm.ArrayType(IntData(sizes.size), IndexType()),
        llvm.ArrayType(IntData(sizes.size), IndexType()),
      )
    )
    val poison = llvm.Poison(Result(descTy))
    emit(block, poison)
    def ins(v: Value[Attribute], agg: Value[Attribute], path: Int*): Value[Attribute] =
      val op = llvm.InsertValue(v.asInstanceOf[Operand[Attribute]], agg.asInstanceOf[Operand[Attribute]], densePath(path*), Result(descTy))
      emit(block, op)
      op.res
    var desc: Value[Attribute] = poison.res
    desc = ins(allocatedPtr, desc, 0)
    desc = ins(alignedPtr, desc, 1)
    desc = ins(offset, desc, 2)
    sizes.zipWithIndex.foreach { case (s, i) => desc = ins(s, desc, 3, i) }
    strides.zipWithIndex.foreach { case (s, i) => desc = ins(s, desc, 4, i) }
    desc

  private def materializeRankedDims(ty: RankedMemrefType, dynamicSizes: Seq[Value[Attribute]], block: Block): Seq[Value[Attribute]] =
    var dynIdx = 0
    ty.shape.attrValues.map { dim =>
      if dim.data >= 0 then constIndex(dim.data, block)
      else
        val v = remap(dynamicSizes(dynIdx))
        dynIdx += 1
        v
    }

  private def defaultStrides(dims: Seq[Value[Attribute]], block: Block): Seq[Value[Attribute]] =
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
      ty: RankedMemrefType,
      dynamicSizes: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    val dims = materializeRankedDims(ty, dynamicSizes.map(remap), block)
    val offset = constIndex(0, block)
    val strides = defaultStrides(dims, block)
    val numElems =
      if dims.isEmpty then constIndex(1, block)
      else dims.tail.foldLeft(dims.head) { (acc, dim) =>
        val mul = llvm.Mul(asIndex(acc), asIndex(dim), Result(IndexType()))
        emit(block, mul)
        mul.res
      }
    val nullPtr = llvm.Zero(Result(llvm.Ptr()))
    emit(block, nullPtr)
    val sizePtr = llvm.GetElementPtr(asPtr(nullPtr.res), Seq(asIndex(numElems)), Result(llvm.Ptr()), dynamicIndexSentinel, ty.elementType)
    emit(block, sizePtr)
    val sizeBytes = llvm.PtrToInt(asPtr(sizePtr.res), Result(IndexType()))
    emit(block, sizeBytes)
    val malloc = llvm.Call(SymbolRefAttr(StringData("malloc")), Seq(sizeBytes.out.asInstanceOf[Operand[Attribute]]), Seq(Result(llvm.Ptr())))
    emit(block, malloc)
    buildDescriptor(malloc.resultss.head, malloc.resultss.head, offset, dims, strides, block)

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
    val allocated = lowerExtract(src, Seq(0), llvm.Ptr(), block)
    val aligned = lowerExtract(src, Seq(1), llvm.Ptr(), block)
    buildDescriptor(allocated, aligned, offset, sizes, strides, block)

  private def lowerReinterpret(op: memref.ReinterpretCast, block: Block): Value[Attribute] =
    val src = remap(op.src)
    val targetTy = op.res.typ.asInstanceOf[RankedMemrefType]
    lowerReinterpret(
      src,
      targetTy,
      remap(op.offset),
      op.sizes.map(remap),
      op.strides.map(remap),
      block,
    )

  private def lowerLoad(
      desc: Value[Attribute],
      ty: RankedMemrefType,
      idxs: Seq[Value[Attribute]],
      resultTy: Attribute,
      block: Block,
  ): Value[Attribute] =
    val base = lowerExtract(desc, Seq(1), llvm.Ptr(), block)
    val layout = ty.encoding.collect { case s: StridedLayoutAttr => s }
    val flagged = layout.exists(_.offset.data == 0) && idxs.size == 2
    val terms = idxs.zipWithIndex.map { case (idx, axis) =>
      val stride = lowerExtract(desc, Seq(4, axis), IndexType(), block)
      val mul = llvm.Mul(asIndex(idx), asIndex(stride), Result(IndexType()), if flagged then Some(overflowNSWNuw) else None)
      emit(block, mul)
      mul.res
    }
    val linear = terms.reduce { (l, r) =>
      val add = llvm.Add(asIndex(l), asIndex(r), Result(IndexType()), if flagged then Some(overflowNSWNuw) else None)
      emit(block, add)
      add.res
    }
    val gep = llvm.GetElementPtr(asPtr(base), Seq(asIndex(linear)), Result(llvm.Ptr()), dynamicIndexSentinel, ty.elementType, if flagged then Some(gepInboundsNuw) else None)
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

  private def lowerExtractStridedMetadata(
      op: memref.ExtractStridedMetadata,
      block: Block,
  ): Seq[Value[Attribute]] =
    val src = remap(op.source)
    val srcTy = op.source.typ.asInstanceOf[RankedMemrefType]
    val rank = srcTy.shape.attrValues.size
    def used(v: Value[Attribute]): Boolean =
      v.uses.nonEmpty || v.typeUses.nonEmpty

    val base =
      if used(op.results.head) then
        src
      else src
    val offset =
      if used(op.results(1)) then lowerExtract(src, Seq(2), IndexType(), block)
      else constIndex(0, block)
    val sizes = (0 until rank).map { i =>
      if used(op.results(2 + i)) then lowerExtract(src, Seq(3, i), IndexType(), block)
      else constIndex(0, block)
    }
    val strides = (0 until rank).map { i =>
      if used(op.results(2 + rank + i)) then lowerExtract(src, Seq(4, i), IndexType(), block)
      else constIndex(0, block)
    }
    Seq(base, offset) ++ sizes ++ strides

  private def lowerDealloc(desc: Value[Attribute], block: Block): Unit =
    val ptr = lowerExtract(desc, Seq(0), llvm.Ptr(), block)
    emit(block, llvm.Call(SymbolRefAttr(StringData("free")), Seq(ptr.asInstanceOf[Operand[Attribute]]), Seq.empty))

  def lower(): func.Func =
    val newBlocks = funcOp.body.blocks.map { oldBlock =>
      val nb = Block(oldBlock.arguments.map(_.typ), Seq.empty)
      blockMap(oldBlock) = nb
      valueMap.addAll(oldBlock.arguments.zip(nb.arguments))
      nb
    }
    funcOp.body.blocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case c: llvm.Constant =>
          val copied = c.deepCopy(using blockMap, valueMap).asInstanceOf[llvm.Constant]
          emit(newBlock, copied)
          valueMap(c.res) = copied.res
          c.value match
            case IntegerAttr(IntData(v), _: IndexType) if v == 1 => cachedOne = Some(copied.res)
            case IntegerAttr(IntData(v), _: IndexType) if v == 0 => cachedZero = Some(copied.res)
            case _                                               => ()
        case op: memref.Alloc =>
          valueMap(op.memref) = lowerAlloc(op, newBlock)
        case op: memref.ReinterpretCast =>
          valueMap(op.res) = lowerReinterpret(op, newBlock)
        case op: memref.ExtractStridedMetadata =>
          valueMap.addAll(op.results.zip(lowerExtractStridedMetadata(op, newBlock)))
        case op: memref.Load =>
          valueMap(op.result) = lowerLoad(op, newBlock)
        case op: memref.Dealloc =>
          lowerDealloc(remap(op.memref), newBlock)
        case ret: func.Return =>
          emit(newBlock, llvm.Return(ret._operands.map(v => remap(v).asInstanceOf[Operand[Attribute]])))
        case other =>
          val copied = other.deepCopy(using blockMap, valueMap)
          emit(newBlock, copied)
          valueMap.addAll(other.results.zip(copied.results))
      }
    }
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(newBlocks))

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: memref.Alloc | _: memref.ReinterpretCast | _: memref.ExtractStridedMetadata | _: memref.Load | _: memref.Dealloc | _: func.Return =>
          true
        case _ => false
      }) =>
    Builder(op).lower()
}

final class FinalizeDynamicMemrefToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "finalize-dynamic-memref-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))
