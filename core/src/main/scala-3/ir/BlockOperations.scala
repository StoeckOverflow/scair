package scair.ir

import scair.helpers.*
import scair.ir.*
import scair.utils.IntrusiveList

//
// ██████╗░ ██╗░░░░░ ░█████╗░ ░█████╗░ ██╗░░██╗
// ██╔══██╗ ██║░░░░░ ██╔══██╗ ██╔══██╗ ██║░██╔╝
// ██████╦╝ ██║░░░░░ ██║░░██║ ██║░░╚═╝ █████═╝░
// ██╔══██╗ ██║░░░░░ ██║░░██║ ██║░░██╗ ██╔═██╗░
// ██████╦╝ ███████╗ ╚█████╔╝ ╚█████╔╝ ██║░╚██╗
// ╚═════╝░ ╚══════╝ ░╚════╝░ ░╚════╝░ ╚═╝░░╚═╝
//
// ░█████╗░ ██████╗░ ███████╗ ██████╗░ ░█████╗░ ████████╗ ██╗ ░█████╗░ ███╗░░██╗ ░██████╗
// ██╔══██╗ ██╔══██╗ ██╔════╝ ██╔══██╗ ██╔══██╗ ╚══██╔══╝ ██║ ██╔══██╗ ████╗░██║ ██╔════╝
// ██║░░██║ ██████╔╝ █████╗░░ ██████╔╝ ███████║ ░░░██║░░░ ██║ ██║░░██║ ██╔██╗██║ ╚█████╗░
// ██║░░██║ ██╔═══╝░ ██╔══╝░░ ██╔══██╗ ██╔══██║ ░░░██║░░░ ██║ ██║░░██║ ██║╚████║ ░╚═══██╗
// ╚█████╔╝ ██║░░░░░ ███████╗ ██║░░██║ ██║░░██║ ░░░██║░░░ ██║ ╚█████╔╝ ██║░╚███║ ██████╔╝
// ░╚════╝░ ╚═╝░░░░░ ╚══════╝ ╚═╝░░╚═╝ ╚═╝░░╚═╝ ░░░╚═╝░░░ ╚═╝ ░╚════╝░ ╚═╝░░╚══╝ ╚═════╝░
//

object BlockOperations:

  def apply(elems: Operation*): BlockOperations =
    from(elems)

  def empty: BlockOperations = new BlockOperations

  def from(i: IterableOnce[Operation]) =
    val list = new BlockOperations
    list.addAll(i)

  def unapplySeq(list: BlockOperations): Some[Seq[Operation]] =
    Some(list.toSeq)

class BlockOperations extends IntrusiveList[Operation]:

  private inline def registerTypeUses(
      owner: Operation | Block,
      a: Attribute,
  ): Unit =
    AttributeWalker.foreachValueAttribute(a) { va =>
      val v = va.getVal()
      v.typeUses += TypeUse(owner, va)
    }

  private inline def unregisterTypeUses(
      owner: Operation | Block,
      a: Attribute,
  ): Unit =
    AttributeWalker.foreachValueAttribute(a) { va =>
      val v = va.getVal()
      v.typeUses -= TypeUse(owner, va)
    }

  private inline def registerOperationTypeUses(op: Operation): Unit =
    op.results.foreach(r => registerTypeUses(op, r.typ))
    op.operands.foreach(v => registerTypeUses(op, v.typ))
    op.attributes.values.foreach(a => registerTypeUses(op, a))
    op.properties.values.foreach(a => registerTypeUses(op, a))

  private inline def unregisterOperationTypeUses(op: Operation): Unit =
    op.results.foreach(r => unregisterTypeUses(op, r.typ))
    op.operands.foreach(v => unregisterTypeUses(op, v.typ))
    op.attributes.values.foreach(a => unregisterTypeUses(op, a))
    op.properties.values.foreach(a => unregisterTypeUses(op, a))

  private inline def handleOperationInsertion(op: Operation) =
    op.operands.foreachWithIndex((o, i) => o.uses += Use(op, i))
    registerOperationTypeUses(op)

  private inline def handleOperationRemoval(op: Operation) =
    op.operands
      .foreachWithIndex((o, i) => o.uses.filterInPlace(_.operation != op))
    unregisterOperationTypeUses(op)

  override final def addOne(elem: Operation): this.type =
    handleOperationInsertion(elem)
    super.addOne(elem)

  override final def prepend(elem: Operation): this.type =
    handleOperationInsertion(elem)
    super.prepend(elem)

  override final def insert(v: Operation, elem: Operation): Unit =
    super.insert(v, elem)
    handleOperationInsertion(elem)

  override final def subtractOne(elem: Operation): this.type =
    handleOperationRemoval(elem)
    super.subtractOne(elem)

  override final def update(v: Operation, elem: Operation): Unit =
    handleOperationRemoval(v)
    super.update(v, elem)
    handleOperationInsertion(elem)
