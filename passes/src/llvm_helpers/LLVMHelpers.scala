package scair.passes.llvm_helpers

import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_memref
import scair.dialects.llvm
import scair.ir.*

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

  def build(
      allocatedPtr: Value[Attribute],
      alignedPtr: Value[Attribute],
      offset: Value[Attribute],
      sizes: Seq[Value[Attribute]],
      strides: Seq[Value[Attribute]],
      block: Block,
  ): Value[Attribute] =
    val builder = LLVMStructBuilder(descriptorType(sizes.size), block)
    var desc = builder.poison()
    desc = builder.insert(allocatedPtr, desc, Seq(0))
    desc = builder.insert(alignedPtr, desc, Seq(1))
    desc = builder.insert(offset, desc, Seq(2))
    sizes.zipWithIndex.foreach { case (size, i) =>
      desc = builder.insert(size, desc, Seq(3, i))
    }
    strides.zipWithIndex.foreach { case (stride, i) =>
      desc = builder.insert(stride, desc, Seq(4, i))
    }
    desc

final class CachedIndexConstants(block: Block):
  private var cachedOne: Option[Value[Attribute]] = None
  private var cachedZero: Option[Value[Attribute]] = None

  def seed(value: Value[Attribute], literal: BigInt): Unit =
    if literal == 0 then cachedZero = Some(value)
    if literal == 1 then cachedOne = Some(value)

  def constIndex(v: BigInt): Value[Attribute] =
    if v == 0 && cachedZero.nonEmpty then cachedZero.get
    else if v == 1 && cachedOne.nonEmpty then cachedOne.get
    else
      val c = llvm.Constant(idxAttr(v), Result(IndexType()))
      block.addOp(c)
      seed(c.res, v)
      c.res

  def zero(): Value[Attribute] = constIndex(0)
  def one(): Value[Attribute] = constIndex(1)

final class RefinedIndexMaterializer(
    remap: Value[Attribute] => Value[Attribute],
    block: Block,
    cache: CachedIndexConstants,
):
  private def constNat(v: Value[Attribute]): Option[BigInt] =
    v.owner match
      case Some(dTensor.NatConst(IntegerAttr(IntData(k), _), _)) => Some(k)
      case _                                                     => None

  def materializeNatOrIndex(v: Value[Attribute]): Value[Attribute] =
    remap(v) match
      case existing if existing.owner.exists {
            case op: Operation => op.name.startsWith("llvm.")
            case _             => false
          } =>
        existing
      case other =>
        constNat(other).map(cache.constIndex).orElse {
          other.owner.collect {
            case dTensor.IndexToNat(idx, _) =>
              materializeNatOrIndex(idx)
            case dTensor.ShapeToIndex(nat, _) =>
              constNat(nat).map(cache.constIndex).getOrElse(other)
          }
        }.getOrElse(other)

  def materializeLayoutParam(param: d_memref.LayoutParam): Value[Attribute] =
    param match
      case i: IntegerAttr =>
        cache.constIndex(i.value.value)
      case v: ValueAttribute =>
        materializeNatOrIndex(v.getVal())
