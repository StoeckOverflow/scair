package scair.verify

import scair.analysis.DominanceInfo
import scair.ir.*
import scair.utils.Err
import scair.utils.OK

import scala.util.boundary
import scala.util.boundary.break

object SSADominanceCheck extends VerifierCheck:
  override val name: String = "ssa-dominance"

  override def run(root: Operation): OK[Unit] =
    val dom = new DominanceInfo(root)

    def fail(msg: String): OK[Unit] = Err(msg)

    def isEntryBlockArgOfUserRegion(
        v: Value[Attribute],
        user: Operation,
    ): Boolean =
      v.owner match
        case Some(block: Block) =>
          block.containerRegion match
            case Some(region) =>
              region.containerOperation.contains(user) &&
              region.blocks.headOption.contains(block) &&
              block.arguments.contains(v)
            case None => false
        case _ => false

    def checkValue(v: Value[Attribute], user: Operation): OK[Unit] =
      if dom.valueDominates(v, user) || isEntryBlockArgOfUserRegion(v, user)
      then OK(())
      else fail(s"value $v does not dominate its use in op `${user.name}`")

    def checkValueAtBlockArgumentType(
        v: Value[Attribute],
        block: Block,
        argIndex: Int,
    ): OK[Unit] =
      v.owner match
        case Some(ownerBlock: Block) if ownerBlock eq block =>
          val refIndex = block.arguments.indexWhere(_ eq v)
          if refIndex >= 0 && refIndex < argIndex then OK(())
          else fail(s"value $v does not dominate block argument type")
        case _ =>
          if dom.valueDominatesBlockEntry(v, block) then OK(())
          else fail(s"value $v does not dominate block argument type")

    def walkAttr(a: Attribute, user: Operation): OK[Unit] =
      boundary[OK[Unit]] {
        AttributeWalker.foreachValueAttribute(a) { va =>
          checkValue(va.getVal(), user) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }
        OK(())
      }

    def checkOpTypeAndAttrUses(op: Operation): OK[Unit] =
      boundary[OK[Unit]] {
        // result types
        op.results.foreach { r =>
          walkAttr(r.typ, op) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        // operand types
        op.operands.foreach { v =>
          walkAttr(v.typ, op) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        // op attributes
        op.attributes.values.foreach { a =>
          walkAttr(a, op) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        // op properties
        op.properties.values.foreach { a =>
          walkAttr(a, op) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        OK(())
      }

    def checkBlockArgumentTypeUses(block: Block): OK[Unit] =
      boundary[OK[Unit]] {
        block.arguments.zipWithIndex.foreach { (arg, i) =>
          AttributeWalker.foreachValueAttribute(arg.typ) { va =>
            checkValueAtBlockArgumentType(va.getVal(), block, i) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        }
        OK(())
      }

    def walkRegion(r: Region): OK[Unit] =
      boundary[OK[Unit]] {
        r.blocks.foreach { b =>
          checkBlockArgumentTypeUses(b) match
            case e: Err => break(e: OK[Unit])
            case _      => ()

          b.operations.foreach { op =>
            // Check operand dominance at this use site
            op.operands.foreach { v =>
              checkValue(v, op) match
                case e: Err => break(e: OK[Unit])
                case _      => ()
            }

            // Recurse into nested regions
            op.regions.foreach { rr =>
              walkRegion(rr) match
                case e: Err => break(e: OK[Unit])
                case _      => ()
            }

            // Check dominance for type uses in types/attributes
            checkOpTypeAndAttrUses(op) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        }
        OK(())
      }

    boundary[OK[Unit]] {
      root.regions.foreach { r =>
        walkRegion(r) match
          case e: Err => break(e: OK[Unit])
          case _      => ()
      }
      OK(())
    }
