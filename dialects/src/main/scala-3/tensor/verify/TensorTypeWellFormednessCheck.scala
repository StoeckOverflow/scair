package scair.dialects.tensor.verify

import scair.dialects.builtin.*
import scair.dialects.tensor.*
import scair.ir.*
import scair.utils.{Err, OK}
import scair.verify.VerifierCheck

import scala.util.boundary
import scala.util.boundary.break

object TensorTypeWellFormednessCheck extends VerifierCheck:
  override val name: String = "tensor-shapes"

  override def run(root: Operation): OK[Unit] =
    walkOperation(root)

  private def walkAttribute(a: Attribute): OK[Unit] =
    boundary[OK[Unit]]:
      a match
        case t: TensorNatType =>
          t.customVerify() match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        case t: TensorVectorType =>
          t.customVerify() match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        case t: TensorMatrixType =>
          t.customVerify() match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        case t: TensorTensorType =>
          t.customVerify() match
            case e: Err => break(e: OK[Unit])
            case _      => ()
        case _ => ()

      a match
        case pa: ParametrizedAttribute =>
          pa.parameters.foreach {
            case nested: Attribute =>
              walkAttribute(nested) match
                case e: Err => break(e: OK[Unit])
                case _      => ()
            case nestedSeq: Seq[?] =>
              nestedSeq.foreach {
                case nested: Attribute =>
                  walkAttribute(nested) match
                    case e: Err => break(e: OK[Unit])
                    case _      => ()
                case _ => ()
              }
          }
        case ArrayAttribute(attrValues) =>
          attrValues.foreach { nested =>
            walkAttribute(nested) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        case DictionaryAttr(entries) =>
          entries.values.foreach { nested =>
            walkAttribute(nested) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        case _ => ()

      OK(())

  private def walkRegion(r: Region): OK[Unit] =
    boundary[OK[Unit]]:
      r.blocks.foreach { b =>
        b.operations.foreach { op =>
          op.attributes.values.foreach { attr =>
            walkAttribute(attr) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
          op.properties.values.foreach { prop =>
            walkAttribute(prop) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
          op.regions.foreach { nested =>
            walkRegion(nested) match
              case e: Err => break(e: OK[Unit])
              case _      => ()
          }
        }
      }
      OK(())

  private def walkOperation(op: Operation): OK[Unit] =
    boundary[OK[Unit]]:
      op.regions.foreach { r =>
        walkRegion(r) match
          case e: Err => break(e: OK[Unit])
          case _      => ()
      }
      OK(())
