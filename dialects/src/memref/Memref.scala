package scair.dialects.memref

import scair.clair.codegen.*
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.ir.*

//
// ███╗░░░███╗ ███████╗ ███╗░░░███╗ ██████╗░ ███████╗ ███████╗
// ████╗░████║ ██╔════╝ ████╗░████║ ██╔══██╗ ██╔════╝ ██╔════╝
// ██╔████╔██║ █████╗░░ ██╔████╔██║ ██████╔╝ █████╗░░ █████╗░░
// ██║╚██╔╝██║ ██╔══╝░░ ██║╚██╔╝██║ ██╔══██╗ ██╔══╝░░ ██╔══╝░░
// ██║░╚═╝░██║ ███████╗ ██║░╚═╝░██║ ██║░░██║ ███████╗ ██║░░░░░
// ╚═╝░░░░░╚═╝ ╚══════╝ ╚═╝░░░░░╚═╝ ╚═╝░░╚═╝ ╚══════╝ ╚═╝░░░░░
//

case class Alloc(
    dynamicSizes: Seq[Operand[IndexType]],
    symbolOperands: Seq[Operand[IndexType]],
    memref: Result[MemrefType],
    alignment: IntegerAttr,
) extends DerivedOperation["memref.alloc", Alloc]
    derives DerivedOperationCompanion

case class Dealloc(
    memref: Operand[MemrefType]
) extends DerivedOperation["memref.dealloc", Dealloc]
    with AssemblyFormat["$memref attr-dict `:` type($memref)"]
    derives DerivedOperationCompanion

case class Dim(
    memref: Operand[MemrefType],
    index: Operand[IndexType],
    result: Result[IndexType],
) extends DerivedOperation["memref.dim", Dim]
    with NoMemoryEffect derives DerivedOperationCompanion

case class Load(
    memref: Operand[MemrefType],
    indices: Seq[Operand[IndexType]],
    result: Result[Attribute],
) extends DerivedOperation["memref.load", Load]
    derives DerivedOperationCompanion

case class Store(
    value: Operand[Attribute],
    memref: Operand[MemrefType],
    indices: Seq[Operand[IndexType]],
) extends DerivedOperation["memref.store", Store]
    derives DerivedOperationCompanion

case class ReinterpretCast(
    src: Operand[MemrefType],
    offset: Operand[IndexType],
    sizes: Seq[Operand[IndexType]],
    strides: Seq[Operand[IndexType]],
    res: Result[MemrefType],
) extends DerivedOperation["memref.reinterpret_cast", ReinterpretCast]
    derives DerivedOperationCompanion

case class DescriptorAlloc(
    dynamicSizes: Seq[Operand[IndexType]],
    descriptor: Result[Attribute],
    source_type: Attribute,
) extends DerivedOperation["memref.descriptor_alloc", DescriptorAlloc]
    derives DerivedOperationCompanion

case class DescriptorReinterpret(
    _operands: Seq[Operand[Attribute]],
    descriptor: Result[Attribute],
    source_type: Attribute,
    target_type: Attribute,
) extends DerivedOperation["memref.descriptor_reinterpret", DescriptorReinterpret]
    derives DerivedOperationCompanion

case class DescriptorLoad(
    _operands: Seq[Operand[Attribute]],
    result: Result[Attribute],
    source_type: Attribute,
) extends DerivedOperation["memref.descriptor_load", DescriptorLoad]
    derives DerivedOperationCompanion

case class DescriptorDealloc(
    descriptor: Operand[Attribute]
) extends DerivedOperation["memref.descriptor_dealloc", DescriptorDealloc]
    derives DerivedOperationCompanion

val MemrefDialect =
  summonDialect[
    EmptyTuple,
    (
        Alloc,
        Dealloc,
        Load,
        Store,
        Dim,
        ReinterpretCast,
        DescriptorAlloc,
        DescriptorReinterpret,
        DescriptorLoad,
        DescriptorDealloc,
    ),
  ]
