package scair.passes.convert_llvm_export_abi

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_memref
import scair.dialects.llvm
import scair.ir.*
import scair.passes.llvm_helpers.*
import scair.transformations.ModulePass

import scala.collection.mutable

private def legalizeLLVMType(attr: Attribute): Attribute =
  attr match
    case _: IndexType =>
      llvmIndexType
    case llvm.StructType(elems) =>
      llvm.StructType(elems.map(e => legalizeLLVMType(e).asInstanceOf[TypeAttribute]))
    case llvm.ArrayType(size, elem) =>
      llvm.ArrayType(size, legalizeLLVMType(elem).asInstanceOf[TypeAttribute])
    case FunctionType(inputs, outputs) =>
      FunctionType(
        inputs.map(i => legalizeLLVMType(i).asInstanceOf[TypeAttribute]),
        outputs.map(o => legalizeLLVMType(o).asInstanceOf[TypeAttribute]),
      )
    case other =>
      other

private def legalizeLLVMConstant(op: llvm.Constant): llvm.Constant =
  op.value match
    case IntegerAttr(IntData(v), _: IndexType) =>
      llvm.Constant(llvmIndexAttr(v), Result(llvmIndexType))
    case _ =>
      op

private final class FunctionLegalizer(op: llvm.Func):
  private val blockMap = mutable.Map.empty[Block, Block]
  private val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]

  private def legalizeAttr(attr: Attribute): Attribute =
    legalizeLLVMType(attr)

  def lower(): llvm.Func =
    val newBlocks = op.body.blocks.map { oldBlock =>
      val newBlock =
        Block.cloneArgumentTypes(
          oldBlock.arguments,
          Seq.empty,
          legalizeAttr,
        )(using valueMap)
      blockMap(oldBlock) = newBlock
      newBlock
    }

    op.body.blocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case c: llvm.Constant =>
          val copied =
            c.value match
              case IntegerAttr(IntData(_), _: IndexType) =>
                legalizeLLVMConstant(c)
              case _ =>
                c.deepCopy(using blockMap, valueMap).asInstanceOf[llvm.Constant]
          newBlock.addOp(copied)
          valueMap.addAll(c.results.zip(copied.results))
        case other =>
          val copied = other.deepCopy(using blockMap, valueMap)
          newBlock.addOp(copied)
          valueMap.addAll(other.results.zip(copied.results))
      }
    }

    val lowered = llvm.Func(
      op.sym_name,
      legalizeAttr(op.function_type).asInstanceOf[FunctionType],
      op.sym_visibility,
      Region(newBlocks),
    )
    lowered.attributes.addAll(op.attributes)
    lowered.attributes.values.foreach(attr =>
      AttributeWalker.remapTypeUsesInPlace(attr)(using valueMap)
    )
    lowered.attributes.get("scair.original_function_type").foreach { orig =>
      val legalized = legalizeAttr(orig)
      AttributeWalker.remapTypeUsesInPlace(legalized)(using valueMap)
      lowered.attributes.update("scair.original_function_type", legalized)
    }
    lowered

private def originalFunctionType(op: llvm.Func): FunctionType =
  op.attributes.get("scair.original_function_type") match
    case Some(ft: FunctionType) => ft
    case _                      => op.function_type

private def containsInternalABIType(attr: Attribute): Boolean =
  attr match
    case _: dTensor.dTensorNatLikeType | _: d_memref.dMemrefMemrefType =>
      true
    case ValueRefType(ref) =>
      containsInternalABIType(ref.getVal().typ)
    case FunctionType(inputs, outputs) =>
      inputs.exists(containsInternalABIType) || outputs.exists(containsInternalABIType)
    case _ =>
      false

private def isEmitCInterface(op: llvm.Func): Boolean =
  op.attributes.contains("llvm.emit_c_interface")

private val bareInterfaceAttr = "scair.emit_bare_interface"
private val descriptorPointerInterfaceAttr = "scair.emit_descriptor_pointer_interface"

private def isBareInterface(op: llvm.Func): Boolean =
  op.attributes.contains(bareInterfaceAttr)

private def isDescriptorPointerInterface(op: llvm.Func): Boolean =
  op.attributes.contains(descriptorPointerInterfaceAttr)

private def wrapperResultTypes(orig: FunctionType): Seq[TypeAttribute] =
  if orig.outputs.exists(_.isInstanceOf[RankedMemrefType]) then
    throw new Exception("convert-llvm-export-abi does not yet support memref returns")
  orig.outputs.map(o => legalizeLLVMType(o).asInstanceOf[TypeAttribute])

private def wrapperArgTypes(orig: FunctionType): Seq[TypeAttribute] =
  orig.inputs.map {
    case _: RankedMemrefType => llvm.Ptr()
    case other               => legalizeLLVMType(other).asInstanceOf[TypeAttribute]
  }

private def buildCInterfaceWrapper(internal: llvm.Func): llvm.Func =
  val origTy = originalFunctionType(internal)
  if containsInternalABIType(origTy) then
    throw new Exception(
      s"original external ABI metadata for ${internal.sym_name.data} was overwritten: $origTy"
    )
  val args = wrapperArgTypes(origTy)
  val results = wrapperResultTypes(origTy)
  val entry = Block(args, Seq.empty)
  val callArgs = mutable.ArrayBuffer.empty[Operand[Attribute]]
  var wrapperArgIdx = 0
  var internalArgIdx = 0

  origTy.inputs.foreach {
    case ranked: RankedMemrefType =>
      val rank = ranked.shape.attrValues.size
      val descTy = RankedMemrefDescriptorHelper.descriptorType(rank)
      val wrapperPtr = entry.arguments(wrapperArgIdx).asInstanceOf[Operand[llvm.Ptr]]
      wrapperArgIdx += 1
      internal.function_type.inputs.lift(internalArgIdx) match
        case Some(structTy: llvm.StructType) if RankedMemrefDescriptorHelper.rankOfDescriptorType(structTy).contains(rank) =>
          val load = llvm.Load(wrapperPtr, Result(descTy))
          entry.addOp(load)
          callArgs += load.res.asInstanceOf[Operand[Attribute]]
          internalArgIdx += 1
        case Some(_: IntegerType) =>
          val load = llvm.Load(wrapperPtr, Result(descTy))
          entry.addOp(load)
          val desc = RankedMemrefDescriptorHelper(load.res, rank, entry)
          (0 until rank).foreach { i =>
            callArgs += desc.size(i).asInstanceOf[Operand[Attribute]]
          }
          callArgs += desc.alignedPtr().asInstanceOf[Operand[Attribute]]
          internalArgIdx += rank + 1
        case other =>
          throw new Exception(
            s"unsupported internal ABI for memref argument in ${internal.sym_name.data}: $other"
          )
    case _ =>
      callArgs += entry.arguments(wrapperArgIdx).asInstanceOf[Operand[Attribute]]
      wrapperArgIdx += 1
      internalArgIdx += 1
  }

  val call = llvm.Call(
    SymbolRefAttr(internal.sym_name),
    callArgs.toSeq,
    internal.function_type.outputs.map(Result(_)),
  )
  entry.addOp(call)
  entry.addOp(llvm.Return(call.results.map(_.asInstanceOf[Operand[Attribute]])))

  llvm.Func(
    StringData(s"_mlir_ciface_${internal.sym_name.data}"),
    FunctionType(args, results),
    None,
    Region(entry),
  )

private final class DescriptorPointerInterfaceBuilder(internal: llvm.Func):
  private val blockMap = mutable.Map.empty[Block, Block]
  private val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]

  def build(): llvm.Func =
    val origTy = originalFunctionType(internal)
    if containsInternalABIType(origTy) then
      throw new Exception(
        s"original external ABI metadata for ${internal.sym_name.data} was overwritten: $origTy"
      )

    val argTypes = wrapperArgTypes(origTy)
    val newBlocks = internal.body.blocks.zipWithIndex.map { case (oldBlock, idx) =>
      val block =
        if idx == 0 then Block(argTypes, Seq.empty)
        else Block.cloneArgumentTypes(oldBlock.arguments, Seq.empty)(using valueMap)
      blockMap(oldBlock) = block
      block
    }

    internal.body.blocks.zip(newBlocks).zipWithIndex.foreach { case ((oldBlock, newBlock), idx) =>
      if idx == 0 then
        var wrapperArgIdx = 0
        var internalArgIdx = 0
        origTy.inputs.foreach {
          case ranked: RankedMemrefType =>
            val rank = ranked.shape.attrValues.size
            val ptrArg = newBlock.arguments(wrapperArgIdx).asInstanceOf[Operand[llvm.Ptr]]
            wrapperArgIdx += 1
            internal.function_type.inputs.lift(internalArgIdx) match
              case Some(structTy: llvm.StructType)
                  if RankedMemrefDescriptorHelper.rankOfDescriptorType(structTy).contains(rank) =>
                val load = llvm.Load(ptrArg, Result(structTy))
                newBlock.addOp(load)
                valueMap(oldBlock.arguments(internalArgIdx)) =
                  load.res.asInstanceOf[Value[Attribute]]
                internalArgIdx += 1
              case other =>
                throw new Exception(
                  s"descriptor-pointer interface only supports baseline descriptor ABI in ${internal.sym_name.data}: $other"
                )
          case _ =>
            valueMap(oldBlock.arguments(internalArgIdx)) =
              newBlock.arguments(wrapperArgIdx).asInstanceOf[Value[Attribute]]
            wrapperArgIdx += 1
            internalArgIdx += 1
        }
      else
        valueMap.addAll(oldBlock.arguments.zip(newBlock.arguments))

      oldBlock.operations.foreach { op =>
        val copied = op.deepCopy(using blockMap, valueMap)
        newBlock.addOp(copied)
        valueMap.addAll(op.results.zip(copied.results))
      }
    }

    val lowered = llvm.Func(
      internal.sym_name,
      FunctionType(argTypes, internal.function_type.outputs),
      internal.sym_visibility,
      Region(newBlocks),
    )
    lowered.attributes.addAll(internal.attributes)
    lowered.attributes.remove(bareInterfaceAttr)
    lowered.attributes.remove(descriptorPointerInterfaceAttr)
    lowered.attributes.remove("llvm.emit_c_interface")
    lowered

private def collectRequiredRuntimeDecls(op: Operation): Seq[String] =
  val found = mutable.LinkedHashSet.empty[String]

  def visit(op: Operation): Unit =
    op match
      case llvm.Call(SymbolRefAttr(StringData(name), _), _, _) =>
        if name == mallocRuntimeName || name == freeRuntimeName then found += name
      case _ => ()
    op.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

  visit(op)
  found.toSeq

final class ConvertLLVMExportABI(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "convert-llvm-export-abi"

  override def transform(op: Operation): Operation =
    op match
      case module: ModuleOp =>
        val newTop = Block(Seq.empty, Seq.empty)
        val requiredRuntimeDecls = mutable.LinkedHashSet.empty[String]
        val existingTopLevelSyms = mutable.LinkedHashSet.empty[String]
        module.body.blocks.foreach { block =>
          block.operations.foreach {
            case funcOp: llvm.Func =>
              val legalized = FunctionLegalizer(funcOp).lower()
              existingTopLevelSyms += legalized.sym_name.data
              requiredRuntimeDecls ++= collectRequiredRuntimeDecls(legalized)
              if isBareInterface(legalized) then
                legalized.attributes.remove(bareInterfaceAttr)
              val cInterfaceWrapper =
                if isEmitCInterface(legalized) && !isDescriptorPointerInterface(
                    legalized
                  ) && legalized.body.blocks.nonEmpty
                then Some(buildCInterfaceWrapper(legalized))
                else None
              val exported =
                if isDescriptorPointerInterface(legalized) && legalized.body.blocks.nonEmpty then
                  DescriptorPointerInterfaceBuilder(legalized).build()
                else
                  legalized
              exported.attributes.remove("scair.original_function_type")
              newTop.addOp(exported)
              cInterfaceWrapper.foreach { wrapper =>
                wrapper.attributes.remove("scair.original_function_type")
                newTop.addOp(wrapper)
              }
            case other =>
              newTop.addOp(other.deepCopy.asInstanceOf[Operation])
          }
        }
        requiredRuntimeDecls.toSeq.reverse.foreach { name =>
          if !existingTopLevelSyms.contains(name) then
            newTop.operations.headOption match
              case Some(first) => newTop.insertOpBefore(first, llvmRuntimeDecl(name))
              case None        => newTop.addOp(llvmRuntimeDecl(name))
        }
        ModuleOp(Region(newTop))
      case other =>
        other
