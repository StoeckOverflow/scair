package scair.dialects.dlam

import scair.ir.*
import scair.Printer
import scair.AttrParser
import fastparse.ParsingRun
import fastparse.*
import scair.Parser

// ---------- NatExprExpr (value-aware naturals) ----------

sealed trait NatExprExpr

final case class NELit(n: Long) extends NatExprExpr
final case class NEAdd(a: NatExprExpr, b: NatExprExpr) extends NatExprExpr
final case class NEMul(a: NatExprExpr, b: NatExprExpr) extends NatExprExpr

// After resolution:
final case class NEFromValue(id: SSAValueId) extends NatExprExpr

// Before resolution (what the parser builds):
final case class NENamedValueRef(ssaName: String) extends NatExprExpr

// ---------- DepTypeExpr (dependent type layer) ----------

sealed trait DepTypeExpr

final case class TEConst(pure: TypeAttribute) extends DepTypeExpr
final case class TEFun(in: DepTypeExpr, out: DepTypeExpr) extends DepTypeExpr
final case class TEForall(body: DepTypeExpr) extends DepTypeExpr
final case class TEVec(len: NatExprExpr, elem: DepTypeExpr) extends DepTypeExpr

// ---------- SSA value references in types ----------

// After resolution:
final case class TEValueRef(id: SSAValueId) extends DepTypeExpr

// Before resolution (what the parser builds):
final case class TENamedValueRef(ssaName: String) extends DepTypeExpr

// ---------- Bridging attribute ----------

final case class DepType(var expr: DepTypeExpr)
    extends TypeAttribute,
      ParametrizedAttribute:
  override def name = "dlam.dep_type"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq()

  override def custom_print(p: Printer): Unit =
    given indent: Int = 0
    p.print("!dlam.dep<")
    DepTypePrinter.printResolved(expr, p)
    p.print(">")

  override def custom_verify(): Either[String, Unit] = Right(())

object DepType extends AttributeCompanion:
  override def name = "dlam.dep_type"

  override def parse[$: P](p: AttrParser): P[DepType] =
    import scair.AttrParser.whitespace
    import DepTypeParser.*
    P("<" ~ DepTypeExpr(p) ~ ">")
      .map(expr => DepType(expr))

// ---------- Parser helpers ----------
object DepTypeParser:
  import scair.AttrParser.whitespace

  // ---------------------------
  // Parse a %name SSA reference
  // ---------------------------
  def SsaName[$: P]: P[String] =
    P("%" ~ Parser.SuffixId) // SuffixId parses MLIR-like identifiers

  // ---------------------------
  // NatExpr grammar
  // ---------------------------
  // def NatExpr[$: P](p: AttrParser): P[NatExprExpr] =
  //  P(
  //    Parser.DecimalLiteral.map(n => NELit(n.toLong))
  //      | SsaName.map(name => NENamedValueRef(name))
  //      | (NatExpr(p) ~ "+" ~ NatExpr(p)).map { case (a, b) => NEAdd(a, b) }
  //      | (NatExpr(p) ~ "*" ~ NatExpr(p)).map { case (a, b) => NEMul(a, b) }
  //  )

  def NatExpr[$: P](p: AttrParser): P[NatExprExpr] =
    def Atom = P(
      Parser.DecimalLiteral.map(n => NELit(n.toLong))
        | SsaName.map(NENamedValueRef(_))
        | "(" ~ NatExpr(p) ~ ")"
    )

    def Mul = P(Atom ~ (("*" ~ Atom).rep)).map { case (head, rest) =>
      rest.foldLeft(head) { case (acc, rhs) => NEMul(acc, rhs) }
    }

    def Add = P(Mul ~ (("+" ~ Mul).rep)).map { case (head, rest) =>
      rest.foldLeft(head) { case (acc, rhs) => NEAdd(acc, rhs) }
    }

    Add

  // ---------------------------
  // DepTypeExpr grammar
  // ---------------------------
  def DepTypeExpr[$: P](p: AttrParser): P[DepTypeExpr] =
    P(
      // SSA value reference in type position
      SsaName.map(name => TENamedValueRef(name))

      // Pure underlying MLIR type (delegated to AttrParser.Type)
        | p.Type.map { t =>
          TEConst(t.asInstanceOf[TypeAttribute])
        }

        // Fun type: fun<T1 -> T2>
        | ("fun" ~ "<" ~ DepTypeExpr(p) ~ "," ~ DepTypeExpr(p) ~ ">")
          .map { case (a, b) => TEFun(a, b) }

        // Vec type: vec<N, Elem>
        | ("vec" ~ "<" ~ NatExpr(p) ~ "," ~ DepTypeExpr(p) ~ ">")
          .map { case (n, e) => TEVec(n, e) }

        // Forall type: forall<Body>
        | ("forall" ~ "<" ~ DepTypeExpr(p) ~ ">")
          .map(body => TEForall(body))
    )

// ---------- Printer helpers ----------
object DepTypePrinter:

  def printResolved(e: DepTypeExpr, p: Printer)(using
      indent: Int = 0
  ): Unit = e match
    case TEConst(pure) =>
      p.print(pure)

    case TEValueRef(id) =>
      p.print("%", ValueNameResolver.resolve(id, p))

    case TENamedValueRef(name) =>
      p.print("%", name)

    case TEFun(i, o) =>
      p.print("fun<")
      printResolved(i, p)
      p.print(", ")
      printResolved(o, p)
      p.print(">")

    case TEVec(len, el) =>
      p.print("vec<")
      printNat(len, p)
      p.print(", ")
      printResolved(el, p)
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
      case NEFromValue(id) =>
        p.print("%", ValueNameResolver.resolve(id, p))
      case NENamedValueRef(name) =>
        p.print("%", name)

  def print(e: DepTypeExpr, p: Printer)(using indent: Int = 0): Unit =
    p.moduleValueTable match
      case Some(table) =>
        // Resolve TENamedValueRef / NENamedValueRef to TEValueRef / NEFromValue
        val resolved = DepTypeSymbolicResolver.resolveDepTypeExpr(
          e,
          table.module,
          table
        )
        printResolved(resolved, p)

      case None =>
        // No table: just print whatever we have
        printResolved(e, p)

  def printNat(n: NatExprExpr, p: Printer)(using indent: Int = 0): Unit =
    p.moduleValueTable match
      case Some(table) =>
        val resolved = DepTypeSymbolicResolver.resolveNatExpr(
          n,
          table.module,
          table
        )
        printNatResolved(resolved, p)
      case None =>
        printNatResolved(n, p)

object ValueNameResolver:

  /** Resolve an SSAValueId to the *same* name the Printer would give to that
    * value in normal SSA printing.
    *
    * If we can’t find the value or don’t have a ModuleValueTable, we fall back
    * to a stable-but-ugly synthetic name.
    */
  def resolve(id: SSAValueId, p: Printer): String =
    p.moduleValueTable match
      case Some(table) =>
        table.valueOf(id) match
          case Some(v) =>
            // assignValueName returns "%0", "%1", …
            // we want just the bare suffix ("0", "1") so we can add our own '%'
            val full = p.assignValueName(v)
            full.stripPrefix("%")
          case None =>
            // Value disappeared or table out of sync; fallback
            fallbackName(id)
      case None =>
        // No table available (e.g. someone created a Printer by hand)
        fallbackName(id)

  private def fallbackName(id: SSAValueId): String =
    s"v${id.defOpId.path.mkString("_")}_${id.resultIndex}"

// ---------- Small helper to collect ValueIDs ----------
object DepTypeAnalysis:

  def collectValueIds(t: DepTypeExpr): List[SSAValueId] =
    t match
      case TEConst(_)       => Nil
      case TEValueRef(id)   => id :: Nil
      case TEFun(i, o)      => collectValueIds(i) ++ collectValueIds(o)
      case TEForall(b)      => collectValueIds(b)
      case TEVec(len, elem) => collectValueIdsNat(len) ++ collectValueIds(elem)
      case TENamedValueRef(name) =>
        throw new Exception(
          s"DepTypeAnalysis: encountered unresolved TENamedValueRef(%$name). " +
            "Did you forget to run DepTypeSymbolicResolver.resolveAll(module) first?"
        )

  def collectValueIdsNat(n: NatExprExpr): List[SSAValueId] =
    n match
      case NELit(_)        => Nil
      case NEAdd(a, b)     => collectValueIdsNat(a) ++ collectValueIdsNat(b)
      case NEMul(a, b)     => collectValueIdsNat(a) ++ collectValueIdsNat(b)
      case NEFromValue(id) => id :: Nil
      case NENamedValueRef(name) =>
        throw new Exception(
          s"DepTypeAnalysis: encountered unresolved NENamedValueRef(%$name). " +
            "Did you forget to run DepTypeSymbolicResolver.resolveAll(module) first?"
        )
