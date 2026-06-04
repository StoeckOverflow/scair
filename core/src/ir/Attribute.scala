package scair.ir

import fastparse.*
import scair.dialects.builtin.IntegerAttr
import scair.parse.Parser
import scair.print.AssemblyPrinter
import scair.print.Printer
import scair.utils.OK

import java.io.StringWriter

//
// ░█████╗░ ████████╗ ████████╗ ██████╗░ ██╗ ██████╗░ ██╗░░░██╗ ████████╗ ███████╗
// ██╔══██╗ ╚══██╔══╝ ╚══██╔══╝ ██╔══██╗ ██║ ██╔══██╗ ██║░░░██║ ╚══██╔══╝ ██╔════╝
// ███████║ ░░░██║░░░ ░░░██║░░░ ██████╔╝ ██║ ██████╦╝ ██║░░░██║ ░░░██║░░░ █████╗░░
// ██╔══██║ ░░░██║░░░ ░░░██║░░░ ██╔══██╗ ██║ ██╔══██╗ ██║░░░██║ ░░░██║░░░ ██╔══╝░░
// ██║░░██║ ░░░██║░░░ ░░░██║░░░ ██║░░██║ ██║ ██████╦╝ ╚██████╔╝ ░░░██║░░░ ███████╗
// ╚═╝░░╚═╝ ░░░╚═╝░░░ ░░░╚═╝░░░ ╚═╝░░╚═╝ ╚═╝ ╚═════╝░ ░╚═════╝░ ░░░╚═╝░░░ ╚══════╝
//

/*≡==--==≡≡≡≡==--=≡≡*\
||    ATTRIBUTES    ||
\*≡==---==≡≡==---==≡*/

sealed trait Attribute:
  def name: String
  def prefix: String = "#"
  def customVerify(): OK[Unit] = OK()
  def printParameters(p: Printer): Unit

  def customPrint(p: Printer): Unit =
    given indentLevel: Int = 0
    p.print(prefix, name)
    printParameters(p)

  override def toString(): String =
    val out = StringWriter()
    val p = AssemblyPrinter(p = out)
    customPrint(p)
    p.flush()
    out.toString()

  /*
   * Return an error message wrapping this attribute. Purposefully shadowing the Err
   * constructor in an Operation's body, to just automatically wrap the error message
   * with the attribute that caused it, without having to explicitly pass 'this' every
   * time.
   */
  def Err(msg: String) = scair.utils.Err(msg, Some(this))

trait TypeAttribute extends Attribute:
  override def prefix: String = "!"

trait IntegerEnumAttr extends Attribute:
  def ordinalIntAttr: IntegerAttr

  override def printParameters(p: Printer): Unit = ()

  override def customPrint(p: Printer): Unit =
    p.print(ordinalIntAttr)

abstract trait ParametrizedAttribute() extends Attribute:

  def parameters: Seq[Attribute | Seq[Attribute]]

  override def printParameters(p: Printer): Unit =
    if parameters.size > 0 then
      p.printListF(
        parameters,
        p.print,
        "<",
        ", ",
        ">",
      )

  override def equals(attr: Any): Boolean =
    attr match
      case x: ParametrizedAttribute =>
        x.name == this.name && x.getClass == this.getClass &&
        x.parameters == this.parameters
      case _ => false

// #value<%x>
// == DataAttribute[Value[Attribute]]
class ValueAttribute(
    var v: Value[Attribute]
) extends Attribute:

  override val name = "value"

  override def printParameters(p: Printer) =
    p.print("<", v, ">") // <%x>

  def getVal(): Value[Attribute] =
    return v

  def replaceValue(
      oldValue: Value[Attribute],
      newValue: Value[Attribute],
  ): Unit =
    if v eq oldValue then v = newValue

  override def customPrint(p: Printer): Unit =
    given indentLevel: Int = 0
    p.print(v) // %x

  override def equals(attr: Any): Boolean =
    attr match
      case x: ValueAttribute => x.getVal().eq(v)
      case _                 => false

  override def hashCode(): Int =
    System.identityHashCode(v)

final case class ValueRefType(ref: ValueAttribute)
    extends TypeAttribute
    with ParametrizedAttribute:

  override val name: String = "value"

  def value: Value[Attribute] = ref.getVal()

  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(ref)

object DataAttribute:
  // Make all DataAttributes implicitely convertible to their held data.
  given [D]: Conversion[DataAttribute[D], D] = _.data

abstract class DataAttribute[D](
    override val name: String,
    val data: D,
) extends Attribute:

  override def printParameters(p: Printer) =
    p.print("<", data.toString, ">")

  override def equals(attr: Any): Boolean =
    attr match
      case x: DataAttribute[?] =>
        x.name == this.name && x.getClass == this.getClass &&
        x.data == this.data
      case _ => false

trait AttributeCompanion[T <: Attribute]:
  def name: String
  def parse[$: P](using Parser): P[T]
  export scair.parse.whitespace

trait AliasedAttribute(val alias: String) extends Attribute

object AttributeWalker:

  private def clonePayload(payload: Any): Any =
    payload match
      case a: Attribute   => cloneValueAttributes(a)
      case xs: Seq[?]     => xs.map(clonePayload)
      case m: Map[?, ?]   => m.map { case (k, v) => k -> clonePayload(v) }
      case opt: Option[?] => opt.map(clonePayload)
      case other          => other

  private def foreachPayloadValueAttribute(
      payload: Any,
      f: ValueAttribute => Unit,
  ): Unit =
    payload match
      case a: Attribute   => foreachValueAttribute(a)(f)
      case xs: Seq[?]     => xs.foreach(foreachPayloadValueAttribute(_, f))
      case m: Map[?, ?]   => m.values.foreach(foreachPayloadValueAttribute(_, f))
      case opt: Option[?] => opt.foreach(foreachPayloadValueAttribute(_, f))
      case _              => ()

  def cloneValueAttributes(a: Attribute): Attribute =
    a match
      case va: ValueAttribute =>
        new ValueAttribute(va.getVal())
      case p: ParametrizedAttribute =>
        p match
          case product: Product =>
            val ctorOpt =
              a.getClass.getConstructors
                .find(_.getParameterCount == product.productArity)
            ctorOpt match
              case Some(ctor) =>
                val args =
                  product.productIterator.map(clonePayload)
                    .map(_.asInstanceOf[Object])
                    .toArray
                ctor.newInstance(args*).asInstanceOf[Attribute]
              case None =>
                a
          case _ =>
            a
      case _ =>
        a

  private def foreachValueAttributeInParams(
      params: Seq[Attribute | Seq[Attribute]],
      f: ValueAttribute => Unit,
  ): Unit =
    params.foreach(foreachPayloadValueAttribute(_, f))

  def foreachValueAttribute(a: Attribute)(f: ValueAttribute => Unit): Unit =
    a match
      case v: ValueAttribute => f(v)
      case _                 => ()

    a match
      case pa: ParametrizedAttribute =>
        foreachValueAttributeInParams(pa.parameters, f)
      case da: DataAttribute[?] =>
        foreachPayloadValueAttribute(da.data, f)
      case _ => ()

  def remapTypeUsesInPlace(
      a: Attribute
  )(using
      valueMapper: collection.mutable.Map[Value[Attribute], Value[Attribute]]
  ): Unit =
    foreachValueAttribute(a) { va =>
      valueMapper.get(va.getVal()) match
        case Some(newV) => va.replaceValue(va.getVal(), newV)
        case None       => ()
    }

  def valueAttributesOf(a: Attribute): Seq[ValueAttribute] =
    val buf = collection.mutable.ArrayBuffer.empty[ValueAttribute]
    foreachValueAttribute(a)(buf += _)
    buf.toSeq
