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

    // Runs after dominance and mirrors the IsolatedFromAbove operand rule for
    // SSA references inside value-dependent type information.
    def checkIsolatedFromAboveBoundary(
        v: Value[Attribute],
        currentIso: Option[Operation],
    ): OK[Unit] =
      currentIso match
        case None => OK(())
        case Some(iso) =>
          v.owner match
            case Some(owner) if iso.isAncestor(owner) => OK(())
            case _ =>
              fail(
                s"value $v crosses IsolatedFromAbove boundary in value-dependent type reference"
              )

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

    def checkValueInAttr(
        v: Value[Attribute],
        user: Operation,
        currentIso: Option[Operation],
    ): OK[Unit] =
      checkValue(v, user) match
        case e: Err => e
        case _      => checkIsolatedFromAboveBoundary(v, currentIso)

    def checkValueInBlockArgumentType(
        v: Value[Attribute],
        block: Block,
        argIndex: Int,
        currentIso: Option[Operation],
    ): OK[Unit] =
      checkValueAtBlockArgumentType(v, block, argIndex) match
        case e: Err => e
        case _      => checkIsolatedFromAboveBoundary(v, currentIso)

    def walkAttr(
        a: Attribute,
        user: Operation,
        currentIso: Option[Operation],
    ): OK[Unit] =
      boundary[OK[Unit]] {
        AttributeWalker.foreachValueAttribute(a) { va =>
          checkValueInAttr(va.getVal(), user, currentIso) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }
        OK(())
      }

    def checkOpTypeAndAttrUses(
        op: Operation,
        currentIso: Option[Operation],
    ): OK[Unit] =
      boundary[OK[Unit]] {
        // result types
        op.results.foreach { r =>
          walkAttr(r.typ, op, currentIso) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        // operand types
        op.operands.foreach { v =>
          walkAttr(v.typ, op, currentIso) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        // op attributes
        op.attributes.values.foreach { a =>
          walkAttr(a, op, currentIso) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        // op properties
        op.properties.values.foreach { a =>
          walkAttr(a, op, currentIso) match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        }

        OK(())
      }

    def checkBlockArgumentTypeUses(
        block: Block,
        currentIso: Option[Operation],
    ): OK[Unit] =
      boundary[OK[Unit]] {
        block.arguments.zipWithIndex.foreach { (arg, i) =>
          AttributeWalker.foreachValueAttribute(arg.typ) { va =>
            checkValueInBlockArgumentType(va.getVal(), block, i, currentIso) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        }
        OK(())
      }

    def walkRegion(r: Region, currentIso: Option[Operation]): OK[Unit] =
      boundary[OK[Unit]] {
        r.blocks.foreach { b =>
          checkBlockArgumentTypeUses(b, currentIso) match
            case e: Err => break(e: OK[Unit])
            case _      => ()

          b.operations.foreach { op =>
            // Check operand dominance at this use site
            op.operands.foreach { v =>
              checkValue(v, op) match
                case e: Err => break(e: OK[Unit])
                case _      => ()
            }

            // Recurse into nested regions, carrying the active isolation boundary.
            val childIso = op match
              case iso: IsolatedFromAbove => Some(iso: Operation)
              case _                      => currentIso
            op.regions.foreach { rr =>
              walkRegion(rr, childIso) match
                case e: Err => break(e: OK[Unit])
                case _      => ()
            }

            // Check dominance for type uses in types/attributes
            checkOpTypeAndAttrUses(op, currentIso) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        }
        OK(())
      }

    boundary[OK[Unit]] {
      val rootIso = root match
        case iso: IsolatedFromAbove => Some(iso: Operation)
        case _                      => None
      root.regions.foreach { r =>
        walkRegion(r, rootIso) match
          case e: Err => break(e: OK[Unit])
          case _      => ()
      }
      OK(())
    }
