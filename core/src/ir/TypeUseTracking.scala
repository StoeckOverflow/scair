package scair.ir

object TypeUseTracking:

  def register(owner: Operation | Block, a: Attribute): Unit =
    AttributeWalker.foreachValueAttribute(a) { va =>
      va.getVal().typeUses += TypeUse(owner, va)
    }

  def unregister(owner: Operation | Block, a: Attribute): Unit =
    AttributeWalker.foreachValueAttribute(a) { va =>
      va.getVal().typeUses -= TypeUse(owner, va)
    }

  def registerOperation(op: Operation): Unit =
    op.results.foreach(r => register(op, r.typ))
    op.operands.foreach(v => register(op, v.typ))
    op.attributes.values.foreach(register(op, _))
    op.properties.values.foreach(register(op, _))

  def unregisterOperation(op: Operation): Unit =
    op.results.foreach(r => unregister(op, r.typ))
    op.operands.foreach(v => unregister(op, v.typ))
    op.attributes.values.foreach(unregister(op, _))
    op.properties.values.foreach(unregister(op, _))

  def registerBlockArgument(block: Block, arg: Value[Attribute]): Unit =
    register(block, arg.typ)

  def unregisterBlockArgument(block: Block, arg: Value[Attribute]): Unit =
    unregister(block, arg.typ)
