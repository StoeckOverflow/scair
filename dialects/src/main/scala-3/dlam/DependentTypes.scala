package scair.dialects.dlam

import scair.ir.*
import scair.Printer
import scair.parse.*
import fastparse.ParsingRun
import fastparse.*

// ---------- NatExprExpr (value-aware naturals) ----------

sealed trait NatExprExpr

final case class NELit(n: Long) extends NatExprExpr
final case class NEAdd(a: NatExprExpr, b: NatExprExpr) extends NatExprExpr
final case class NEMul(a: NatExprExpr, b: NatExprExpr) extends NatExprExpr
final case class NEFromValue(id: Value[Attribute]) extends NatExprExpr

// ---------- DepTypeExpr (dependent type layer) ----------

sealed trait DepTypeExpr

final case class TEConst(pure: TypeAttribute) extends DepTypeExpr
final case class TEFun(in: DepTypeExpr, out: DepTypeExpr) extends DepTypeExpr
final case class TEForall(body: DepTypeExpr) extends DepTypeExpr
final case class TEValueRef(id: Value[Attribute]) extends DepTypeExpr

// ---------- Bridging attribute ----------

final case class DepType(var expr: DepTypeExpr)
    extends TypeAttribute,
      ParametrizedAttribute:
  override def name = "dlam.dep"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq()

  override def customPrint(p: Printer): Unit =
    given indent: Int = 0
    p.print("!dlam.dep<")
    DepTypePrinter.print(expr, p)
    p.print(">")

given AttributeCompanion[DepType]:
  override def name = "dlam.dep"

  override def parse[$: P](using Parser): P[DepType] =
    import DepTypeParser.*
    P("<" ~ DepTypeExpr ~ ">").map(expr => DepType(expr))

// ---------- Parser helpers ----------
object DepTypeParser:
  // import scair.parse.whitespace
  import scair.parse.whitespace

  // ---------------------------
  // Parse a %name SSA reference
  // ---------------------------

  def valueRef[$: P](using Parser): P[Value[Attribute]] =
    operandNameP.flatMap(operandP(_, DlamTypeType()))

  // ---------------------------
  // NatExpr grammar
  // ---------------------------

  def NatExpr[$: P](using Parser): P[NatExprExpr] =
    def Lit: P[NatExprExpr] =
      decimalLiteralP.map(n => NELit(n.toLong))

    def Val: P[NatExprExpr] =
      valueRef.map(NEFromValue(_))

    def Atom: P[NatExprExpr] =
      P(Lit | Val | "(" ~ NatExpr ~ ")")

    def Mul: P[NatExprExpr] =
      P(Atom ~ (("*" ~ Atom).rep)).map { case (head, rest) =>
        rest.foldLeft(head) { case (acc, rhs) => NEMul(acc, rhs) }
      }

    def Add: P[NatExprExpr] =
      P(Mul ~ (("+" ~ Mul).rep)).map { case (head, rest) =>
        rest.foldLeft(head) { case (acc, rhs) => NEAdd(acc, rhs) }
      }

    // parse first, then validate
    Add.flatMap { ast =>
      if isAllowedNatExpr(ast) then Pass(ast)
      else
        Fail.opaque("NatExpr may reference at most one SSA value (no %a + %b)")
    }

  private def countSSA(n: NatExprExpr): Int = n match
    case NELit(_)       => 0
    case NEFromValue(_) => 1
    case NEAdd(a, b)    => countSSA(a) + countSSA(b)
    case NEMul(a, b)    => countSSA(a) + countSSA(b)

  private def isAllowedNatExpr(n: NatExprExpr): Boolean =
    countSSA(n) <= 1

  // ---------------------------
  // DepTypeExpr grammar
  // ---------------------------

  def DepTypeExpr[$: P](using Parser): P[DepTypeExpr] =
    P(
      // SSA value reference in type position
      valueRef.map(value => TEValueRef(value))

      // Pure underlying MLIR type (delegated to AttrParser.Type)
      | typeP.map(t => TEConst(t.asInstanceOf[TypeAttribute]))

      // Fun type: fun<T1 -> T2>
      | ("fun" ~ "<" ~ DepTypeExpr ~ "," ~ DepTypeExpr ~ ">").map {
        case (a, b) => TEFun(a, b)
      }

      // Forall type: forall<Body>
      | ("forall" ~ "<" ~ DepTypeExpr ~ ">").map(body => TEForall(body))
    )

// ---------- Printer helpers ----------
object DepTypePrinter:

  def printResolved(e: DepTypeExpr, p: Printer)(using
      indent: Int = 0
  ): Unit = e match
    case TEConst(pure) =>
      p.print(pure)

    case TEValueRef(v) =>
      p.print(v)

    case TEFun(i, o) =>
      p.print("fun<")
      printResolved(i, p)
      p.print(", ")
      printResolved(o, p)
      p.print(">")

    case TEForall(b) =>
      p.print("forall<")
      printResolved(b, p)
      p.print(">")

  def printNatResolved(n: NatExprExpr, p: Printer)(using
      indent: Int = 0
  ): Unit =
    n match
      case NELit(v)    => p.print(v.toString)
      case NEAdd(a, b) =>
        printNatResolved(a, p); p.print(" + "); printNatResolved(b, p)
      case NEMul(a, b) =>
        printNatResolved(a, p); p.print(" * "); printNatResolved(b, p)
      case NEFromValue(v) =>
        p.print(v)

  def print(e: DepTypeExpr, p: Printer)(using indent: Int = 0): Unit =
    printResolved(e, p)

  def printNat(n: NatExprExpr, p: Printer)(using indent: Int = 0): Unit =
    printNatResolved(n, p)

// ---------- Small helper to collect ValueIDs ----------
object DepTypeAnalysis:

  def collectValues(t: DepTypeExpr): List[Value[Attribute]] =
    t match
      case TEConst(_)    => Nil
      case TEValueRef(v) => v :: Nil
      case TEFun(i, o)   => collectValues(i) ++ collectValues(o)
      case TEForall(b)   => collectValues(b)

  def collectValuesNat(n: NatExprExpr): List[Value[Attribute]] =
    n match
      case NELit(_)       => Nil
      case NEAdd(a, b)    => collectValuesNat(a) ++ collectValuesNat(b)
      case NEMul(a, b)    => collectValuesNat(a) ++ collectValuesNat(b)
      case NEFromValue(v) => v :: Nil
