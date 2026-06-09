package scair.passes.analysis

import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.SizeWitnessProvenance

object TailBoundFacts:
  final case class TileEnd(
      tileIv: Value[Attribute],
      tileSize: Value[Attribute],
      value: Value[Attribute],
      prelude: Seq[Operation] = Seq.empty,
  )

  final case class TailClamp(
      tileEnd: TileEnd,
      fullBound: Value[Attribute],
      value: Value[Attribute],
  ):
    def replacementOps: Seq[Operation] = tileEnd.prelude
    def replacementValue: Value[Attribute] = tileEnd.value

  private enum ExprFact:
    case Operand(value: Value[Attribute])
    case End(tileEnd: TileEnd)

  private def asAttr(v: Value[?]): Value[Attribute] =
    v.asInstanceOf[Value[Attribute]]

  private def sameValue(lhs: Value[?], rhs: Value[?]): Boolean =
    lhs.asInstanceOf[AnyRef] eq rhs.asInstanceOf[AnyRef]

  private def affineOperand(
      expr: AffineExpr,
      dimOperands: Seq[Value[Attribute]],
      symbolOperands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    expr match
      case AffineDimExpr(name) =>
        val idx = map.affineMap.dimensions.indexOf(name)
        if idx < 0 then None else dimOperands.lift(idx)
      case AffineSymExpr(name) =>
        val idx = map.affineMap.symbols.indexOf(name)
        if idx < 0 then None else symbolOperands.lift(idx)
      case _ => None

  private def singleExprMap(map: AffineMapAttr, expr: AffineExpr): AffineMapAttr =
    AffineMapAttr(
      AffineMap(
        dimensions = map.affineMap.dimensions,
        symbols = map.affineMap.symbols,
        affineExprs = Seq(expr),
      )
    )

  private def tileEndFromAffineExpr(
      expr: AffineExpr,
      dimOperands: Seq[Value[Attribute]],
      symbolOperands: Seq[Value[Attribute]],
      map: AffineMapAttr,
      replacement: AffineMapAttr => (Seq[Operation], Value[Attribute]),
  ): Option[TileEnd] =
    expr match
      case AffineBinaryOpExpr(AffineBinaryOp.Add, lhsExpr, rhsExpr) =>
        (affineOperand(lhsExpr, dimOperands, symbolOperands, map),
         affineOperand(rhsExpr, dimOperands, symbolOperands, map)) match
          case (Some(lhs), Some(rhs)) =>
            val (prelude, value) = replacement(singleExprMap(map, expr))
            Some(TileEnd(lhs, rhs, value, prelude))
          case _ => None
      case _ => None

  private def exprFactFromAffineExpr(
      expr: AffineExpr,
      dimOperands: Seq[Value[Attribute]],
      symbolOperands: Seq[Value[Attribute]],
      map: AffineMapAttr,
      replacement: AffineMapAttr => (Seq[Operation], Value[Attribute]),
  ): Option[ExprFact] =
    affineOperand(expr, dimOperands, symbolOperands, map)
      .map(v => ExprFact.Operand(v))
      .orElse(
        tileEndFromAffineExpr(expr, dimOperands, symbolOperands, map, replacement)
          .map(ExprFact.End.apply)
      )

  private def clampFromFacts(
      lhs: ExprFact,
      rhs: ExprFact,
      value: Value[Attribute],
  ): Seq[TailClamp] =
    (lhs, rhs) match
      case (ExprFact.End(end), ExprFact.Operand(bound)) => Seq(TailClamp(end, bound, value))
      case (ExprFact.Operand(bound), ExprFact.End(end)) => Seq(TailClamp(end, bound, value))
      case _                                           => Seq.empty

  def tileEnd(value: Value[Attribute]): Seq[TileEnd] =
    value.owner.toSeq.flatMap {
      case op: arith.AddI =>
        val lhs = asAttr(op.lhs)
        val rhs = asAttr(op.rhs)
        Seq(
          TileEnd(lhs, rhs, op.result),
          TileEnd(rhs, lhs, op.result),
        )
      case op: d_affine.Apply =>
        op.map.affineMap.affineExprs.flatMap(expr =>
          tileEndFromAffineExpr(
            expr,
            op.dimOperands.map(asAttr),
            op.symbolOperands.map(asAttr),
            op.map,
            _ => (Seq.empty, op.res),
          )
        )
      case op: affine.Apply =>
        val dimCount = op.map.affineMap.dimensions.size
        val dimOperands = op.mapOperands.take(dimCount).map(asAttr)
        val symbolOperands = op.mapOperands.drop(dimCount).map(asAttr)
        op.map.affineMap.affineExprs.flatMap(expr =>
          tileEndFromAffineExpr(
            expr,
            dimOperands,
            symbolOperands,
            op.map,
            _ => (Seq.empty, op.res),
          )
        )
      case _ => Seq.empty
    }

  def tailClamp(value: Value[Attribute]): Seq[TailClamp] =
    value.owner.toSeq.flatMap {
      case op: arith.MinSI =>
        val lhs = asAttr(op.lhs)
        val rhs = asAttr(op.rhs)
        tileEnd(lhs).map(TailClamp(_, rhs, op.result)) ++
          tileEnd(rhs).map(TailClamp(_, lhs, op.result))
      case op: d_affine.Min =>
        val dimOperands = op.dimOperands.map(asAttr)
        val symbolOperands = op.symbolOperands.map(asAttr)
        val exprs = op.map.affineMap.affineExprs
        if exprs.size != 2 then Seq.empty
        else
          val materialize = (map: AffineMapAttr) =>
            val apply = d_affine.Apply(op.dimOperands, op.symbolOperands, map, Result(IndexType()))
            (Seq(apply), apply.res)
          for
            lhs <- exprFactFromAffineExpr(exprs(0), dimOperands, symbolOperands, op.map, materialize).toSeq
            rhs <- exprFactFromAffineExpr(exprs(1), dimOperands, symbolOperands, op.map, materialize).toSeq
            clamp <- clampFromFacts(lhs, rhs, op.res)
          yield clamp
      case op: affine.Min =>
        val dimCount = op.map.affineMap.dimensions.size
        val dimOperands = op.arguments.take(dimCount).map(asAttr)
        val symbolOperands = op.arguments.drop(dimCount).map(asAttr)
        val exprs = op.map.affineMap.affineExprs
        if exprs.size != 2 then Seq.empty
        else
          val materialize = (map: AffineMapAttr) =>
            val apply = affine.Apply(op.arguments, Result(IndexType()), map)
            (Seq(apply), apply.res)
          for
            lhs <- exprFactFromAffineExpr(exprs(0), dimOperands, symbolOperands, op.map, materialize).toSeq
            rhs <- exprFactFromAffineExpr(exprs(1), dimOperands, symbolOperands, op.map, materialize).toSeq
            clamp <- clampFromFacts(lhs, rhs, op.result)
          yield clamp
      case _ => Seq.empty
    }

  def enclosingDAffineFor(op: Operation): Option[d_affine.For] =
    op.containerBlock
      .flatMap(_.containerRegion)
      .flatMap(_.containerOperation)
      .collect { case loop: d_affine.For => loop }

  private def hasCompatibleStep(loop: d_affine.For, tileSize: Value[Attribute]): Boolean =
    val positiveTileSize =
      SizeWitnessProvenance.exactConst(tileSize).exists(_ > 0) || SizeWitnessProvenance.isPositive(tileSize)
    if !positiveTileSize then false
    else if loop.stepOperands.nonEmpty then
      loop.stepOperands.size == 1 &&
        (SizeWitnessProvenance.equivalentSizeWitnessOrConst(loop.stepOperands.head, tileSize) ||
          SizeProductFacts.sameProductModuloOrder(loop.stepOperands.head, tileSize))
    else
      SizeWitnessProvenance.exactConst(tileSize).contains(loop.step.value.value)

  private def loopCarriesTileIv(loop: d_affine.For, tileIv: Value[Attribute]): Boolean =
    loop.body.blocks.headOption.exists(_.arguments.headOption.exists(sameValue(_, tileIv)))

  private def loopUpperBoundIs(loop: d_affine.For, upperBound: Value[Attribute]): Boolean =
    loop.upperBoundOperands.size == 1 && sameValue(loop.upperBoundOperands.head, upperBound)

  def canDropClamp(clamp: TailClamp, enclosingLoop: d_affine.For): Boolean =
    loopCarriesTileIv(enclosingLoop, clamp.tileEnd.tileIv) &&
      loopUpperBoundIs(enclosingLoop, clamp.fullBound) &&
      hasCompatibleStep(enclosingLoop, clamp.tileEnd.tileSize) &&
      SizeProductFacts.containsExplicitFactor(clamp.fullBound, clamp.tileEnd.tileSize)
