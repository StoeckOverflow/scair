package scair.passes.llvm_helpers

import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_memref
import scair.dialects.llvm
import scair.ir.*
import scala.collection.mutable

private def i32Attr(v: Int): IntegerAttr =
  IntegerAttr(IntData(v), I32)

def densePath(indices: Int*): DenseArrayAttr =
  DenseArrayAttr(I32, indices.map(i => i32Attr(i)))

def dynamicIndexSentinel: DenseArrayAttr =
  DenseArrayAttr(I32, Seq(i32Attr(-2147483648)))

def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

def asPtr(v: Value[Attribute]): Operand[llvm.Ptr] =
  v.asInstanceOf[Operand[llvm.Ptr]]

final class LLVMStructBuilder(
    val structTy: llvm.StructType,
    block: Block,
):
  private def emit(op: Operation): Unit =
    block.addOp(op)

  def poison(): Value[Attribute] =
    val op = llvm.Poison(Result(structTy))
    emit(op)
    op.res

  def extract(
      agg: Value[Attribute],
      path: Seq[Int],
      resultTy: Attribute,
  ): Value[Attribute] =
    val op = llvm.ExtractValue(
      agg.asInstanceOf[Operand[Attribute]],
      densePath(path*),
      Result(resultTy),
    )
    emit(op)
    op.res

  def insert(
      value: Value[Attribute],
      agg: Value[Attribute],
      path: Seq[Int],
  ): Value[Attribute] =
    val op = llvm.InsertValue(
      value.asInstanceOf[Operand[Attribute]],
      agg.asInstanceOf[Operand[Attribute]],
      densePath(path*),
      Result(structTy),
    )
    emit(op)
    op.res

final class RankedMemrefDescriptorHelper(
    val desc: Value[Attribute],
    val rank: Int,
    block: Block,
):
  private val structTy = RankedMemrefDescriptorHelper.descriptorType(rank)
  private val builder = LLVMStructBuilder(structTy, block)

  def allocatedPtr(): Value[Attribute] =
    builder.extract(desc, Seq(0), llvm.Ptr())

  def alignedPtr(): Value[Attribute] =
    builder.extract(desc, Seq(1), llvm.Ptr())

  def offset(): Value[Attribute] =
    builder.extract(desc, Seq(2), IndexType())

  def size(i: Int): Value[Attribute] =
    builder.extract(desc, Seq(3, i), IndexType())

  def stride(i: Int): Value[Attribute] =
    builder.extract(desc, Seq(4, i), IndexType())

  private def updated(nextDesc: Value[Attribute]): RankedMemrefDescriptorHelper =
    RankedMemrefDescriptorHelper(nextDesc, rank, block)

  def setAllocatedPtr(ptr: Value[Attribute]): RankedMemrefDescriptorHelper =
    updated(builder.insert(ptr, desc, Seq(0)))

  def setAlignedPtr(ptr: Value[Attribute]): RankedMemrefDescriptorHelper =
    updated(builder.insert(ptr, desc, Seq(1)))

  def setOffset(off: Value[Attribute]): RankedMemrefDescriptorHelper =
    updated(builder.insert(off, desc, Seq(2)))

  def setSize(i: Int, size: Value[Attribute]): RankedMemrefDescriptorHelper =
    updated(builder.insert(size, desc, Seq(3, i)))

  def setStride(i: Int, stride: Value[Attribute]): RankedMemrefDescriptorHelper =
    updated(builder.insert(stride, desc, Seq(4, i)))

object RankedMemrefDescriptorHelper:
  def descriptorType(rank: Int): llvm.StructType =
    llvm.StructType(
      Seq(
        llvm.Ptr(),
        llvm.Ptr(),
        IndexType(),
        llvm.ArrayType(IntData(rank), IndexType()),
        llvm.ArrayType(IntData(rank), IndexType()),
      )
    )

  def rankOfDescriptorType(attr: Attribute): Option[Int] =
    attr match
      case llvm.StructType(
            Seq(
              _: llvm.Ptr,
              _: llvm.Ptr,
              _: IndexType,
              llvm.ArrayType(IntData(rank), _: IndexType),
              llvm.ArrayType(_, _: IndexType),
            )
          ) =>
        Some(rank.toInt)
      case _ => None

  def poison(rank: Int, block: Block): RankedMemrefDescriptorHelper =
    val builder = LLVMStructBuilder(descriptorType(rank), block)
    RankedMemrefDescriptorHelper(builder.poison(), rank, block)

  def build(
      allocatedPtr: Value[Attribute],
      alignedPtr: Value[Attribute],
      offset: Value[Attribute],
      sizes: Seq[Value[Attribute]],
      strides: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    var desc = poison(sizes.size, block)
    desc = desc.setAllocatedPtr(allocatedPtr)
    desc = desc.setAlignedPtr(alignedPtr)
    desc = desc.setOffset(offset)
    sizes.zipWithIndex.foreach { case (size, i) =>
      desc = desc.setSize(i, size)
    }
    strides.zipWithIndex.foreach { case (stride, i) =>
      desc = desc.setStride(i, stride)
    }
    desc.desc

def buildDefaultStrides(
    dims: Seq[Value[Attribute]],
    block: Block,
    cache: CachedIndexConstants,
): Seq[Value[Attribute]] =
  if dims.isEmpty then Seq.empty
  else
    val one = cache.one(block)
    val rev = mutable.ArrayBuffer[Value[Attribute]](one)
    dims.reverse.drop(1).foreach { dim =>
      val mul = llvm.Mul(asIndex(dim), asIndex(rev.last), Result(IndexType()))
      block.addOp(mul)
      rev += mul.res
    }
    rev.reverse.toSeq

def computeAllocationSizeBytes(
    numElems: Value[Attribute],
    elemTy: Attribute,
    block: Block,
): Value[Attribute] =
  val nullPtr = llvm.Zero(Result(llvm.Ptr()))
  block.addOp(nullPtr)
  val sizePtr = llvm.GetElementPtr(
    asPtr(nullPtr.res),
    Seq(asIndex(numElems)),
    Result(llvm.Ptr()),
    dynamicIndexSentinel,
    elemTy,
  )
  block.addOp(sizePtr)
  val sizeBytes = llvm.PtrToInt(asPtr(sizePtr.res), Result(IndexType()))
  block.addOp(sizeBytes)
  sizeBytes.out

final class CachedIndexConstants(defaultBlock: Block):
  private var cachedOne: Option[Value[Attribute]] = None
  private var cachedZero: Option[Value[Attribute]] = None

  def seed(value: Value[Attribute], literal: BigInt): Unit =
    if literal == 0 then cachedZero = Some(value)
    if literal == 1 then cachedOne = Some(value)

  def constIndex(v: BigInt, block: Block = defaultBlock): Value[Attribute] =
    if v == 0 && cachedZero.nonEmpty then cachedZero.get
    else if v == 1 && cachedOne.nonEmpty then cachedOne.get
    else
      val c = llvm.Constant(idxAttr(v), Result(IndexType()))
      block.addOp(c)
      seed(c.res, v)
      c.res

  def zero(block: Block = defaultBlock): Value[Attribute] = constIndex(0, block)
  def one(block: Block = defaultBlock): Value[Attribute] = constIndex(1, block)

final class RefinedIndexMaterializer(
    remap: Value[Attribute] => Value[Attribute],
    cache: CachedIndexConstants,
):
  private def constNat(v: Value[Attribute]): Option[BigInt] =
    v.owner match
      case Some(dTensor.NatConst(IntegerAttr(IntData(k), _), _)) => Some(k)
      case _                                                     => None

  def materializeNatOrIndex(v: Value[Attribute], block: Block): Value[Attribute] =
    remap(v) match
      case existing if existing.owner.exists {
            case op: Operation => op.name.startsWith("llvm.")
            case _             => false
          } =>
        existing
      case other =>
        constNat(other).map(k => cache.constIndex(k, block)).orElse {
          other.owner.collect {
            case dTensor.IndexToNat(idx, _) =>
              materializeNatOrIndex(idx, block)
            case dTensor.ShapeToIndex(nat, _) =>
              constNat(nat).map(k => cache.constIndex(k, block)).getOrElse(other)
          }
        }.getOrElse(other)

  def materializeLayoutParam(
      param: d_memref.LayoutParam,
      block: Block,
  ): Value[Attribute] =
    param match
      case i: IntegerAttr =>
        cache.constIndex(i.value.value, block)
      case v: ValueAttribute =>
        materializeNatOrIndex(v.getVal(), block)
