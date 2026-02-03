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

    def checkValue(v: Value[Attribute], user: Operation): OK[Unit] =
      if dom.valueDominates(v, user) then OK(())
      else fail(s"value $v does not dominate its use in op `${user.name}`")

    def walkAttr(a: Attribute, user: Operation): OK[Unit] =
      a match

        case pa: ParametrizedAttribute =>
          boundary[OK[Unit]] {
            pa.parameters.foreach {
              case x: Attribute =>
                walkAttr(x, user) match
                  case e: Err => break(e: OK[Unit])
                  case _      => ()

              case v: Value[?] =>
                // parameters may contain SSA values now
                checkValue(v.asInstanceOf[Value[Attribute]], user) match
                  case e: Err => break(e: OK[Unit])
                  case _      => ()

              case xs: Seq[?] =>
                xs.foreach {
                  case x: Attribute =>
                    walkAttr(x, user) match
                      case e: Err => break(e: OK[Unit])
                      case _      => ()
                  case v: Value[?] =>
                    checkValue(v.asInstanceOf[Value[Attribute]], user) match
                      case e: Err => break(e: OK[Unit])
                      case _      => ()
                  case _ => ()
                }
              case _ => ()
            }
            OK(())
          }

        case _ =>
          OK(())

    def checkOpTypePositions(op: Operation): OK[Unit] =
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

        OK(())
      }

    def walkRegion(r: Region): OK[Unit] =
      boundary[OK[Unit]] {
        r.blocks.foreach { b =>
          b.operations.foreach { op =>
            // Check operand dominance at this use site
            op.operands.foreach { v =>
              if !dom.valueDominates(v, op) then
                break(
                  Err(
                    s"value $v does not dominate its use in op `${op.name}`"
                  ): OK[Unit]
                )
            }

            // Recurse into nested regions
            op.regions.foreach { rr =>
              walkRegion(rr) match
                case e: Err => break(e: OK[Unit])
                case _      => ()
            }
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
