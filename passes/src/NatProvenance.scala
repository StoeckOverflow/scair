package scair.passes

import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.dialects.d_affine
import scair.ir.*
import scair.utils.OK

import scala.collection.mutable

object NatProvenance:
  /** Conservative provenance/constant reasoning for the currently supported affine subset;
    * does not attempt general semi-affine symbolic solving.
    */
  private def collectAll[T](xs: Seq[Option[T]]): Option[Seq[T]] =
    if xs.forall(_.isDefined) then Some(xs.flatten) else None

  private def floorDiv(v: BigInt, d: BigInt): Option[BigInt] =
    if d <= 0 then None
    else
      val q = v / d
      val r = v % d
      if r == 0 || v >= 0 then Some(q) else Some(q - 1)

  private def ceilDiv(v: BigInt, d: BigInt): Option[BigInt] =
    if d <= 0 then None
    else
      val q = v / d
      val r = v % d
      if r == 0 then Some(q)
      else if v >= 0 then Some(q + 1)
      else Some(q)

  private def evalAffineExprConst(
      expr: AffineExpr,
      dims: Map[String, BigInt],
      syms: Map[String, BigInt],
  ): Option[BigInt] =
    expr match
      case AffineDimExpr(position)      => dims.get(position)
      case AffineSymExpr(position)      => syms.get(position)
      case AffineConstantExpr(value)    => Some(value)
      case AffineBinaryOpExpr(op, l, r) =>
        for
          lhs <- evalAffineExprConst(l, dims, syms)
          rhs <- evalAffineExprConst(r, dims, syms)
          out <- op match
            case AffineBinaryOp.Add      => Some(lhs + rhs)
            case AffineBinaryOp.Minus    => Some(lhs - rhs)
            case AffineBinaryOp.Multiply => Some(lhs * rhs)
            case AffineBinaryOp.CeilDiv  => ceilDiv(lhs, rhs)
            case AffineBinaryOp.FloorDiv => floorDiv(lhs, rhs)
            case AffineBinaryOp.Mod      => floorDiv(lhs, rhs).map(q => lhs - q * rhs)
        yield out

  private def evalAffineApplyConst(
      args: Seq[Value[Attribute]],
      map: AffineMapAttr,
      eval: Value[Attribute] => Option[BigInt],
  ): Option[BigInt] =
    val dimNames = map.affineMap.dimensions
    val symNames = map.affineMap.symbols
    if map.affineMap.affineExprs.size != 1 then None
    else if args.size != dimNames.size + symNames.size then None
    else
      for
        dimVals <- collectAll(args.take(dimNames.size).map(eval))
        symVals <- collectAll(args.drop(dimNames.size).map(eval))
        out <- evalAffineExprConst(
          map.affineMap.affineExprs.head,
          dimNames.zip(dimVals).toMap,
          symNames.zip(symVals).toMap,
        )
      yield out

  private def recoverProjectedNatFromApply(
      dimOperands: Seq[Value[Attribute]],
      symbolOperands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    if map.affineMap.affineExprs.size != 1 then None
    else
      val dimNames = map.affineMap.dimensions
      val symNames = map.affineMap.symbols
      val dimCount = dimNames.size
      if dimOperands.size != dimCount || symbolOperands.size != symNames.size
      then None
      else
        map.affineMap.affineExprs.head match
          case AffineDimExpr(position) =>
            val idx = dimNames.indexOf(position)
            if idx < 0 then None
            else
              DTensorTypeUtil.resolveNatFromIndexValue(dimOperands(idx)) match
                case OK(nat) => Some(nat)
                case _       => None
          case AffineSymExpr(position) =>
            val idx = symNames.indexOf(position)
            if idx < 0 then None
            else
              DTensorTypeUtil.resolveNatFromIndexValue(symbolOperands(idx)) match
                case OK(nat) => Some(nat)
                case _       => None
          case _ => None

  def resolveNat(v: Value[Attribute]): Option[Value[Attribute]] =
    DTensorTypeUtil.resolveNatProvenance(v) match
      case OK(nat) => Some(nat)
      case _       =>
        v.owner match
          case Some(d_affine.Apply(dimOperands, symbolOperands, map, _)) =>
            recoverProjectedNatFromApply(dimOperands, symbolOperands, map)
          case _ => None

  def sameNat(lhs: Value[Attribute], rhs: Value[Attribute]): Boolean =
    (resolveNat(lhs), resolveNat(rhs)) match
      case (Some(l), Some(r)) => l eq r
      case _                  => false

  def equivalentNatOrConst(lhs: Value[Attribute], rhs: Value[Attribute]): Boolean =
    (resolveNat(lhs), resolveNat(rhs)) match
      case (Some(l), Some(r)) if l eq r => true
      case _ =>
        (exactConst(lhs), exactConst(rhs)) match
          case (Some(l), Some(r)) => l == r
          case _                  => false

  def exactConst(v: Value[Attribute]): Option[BigInt] =
    val memo = mutable.Map.empty[Value[Attribute], Option[BigInt]]
    val inProgress = mutable.Set.empty[Value[Attribute]]

    def eval(x: Value[Attribute]): Option[BigInt] =
      val base = resolveNat(x).getOrElse(x)
      memo.getOrElseUpdate(
        base, {
          if inProgress.contains(base) then None
          else
            inProgress += base
            val out = base.owner match
              case Some(NatConst(IntegerAttr(IntData(c), _), _)) => Some(c)
              case Some(ShapeToIndex(nat, _))                    => eval(nat)
              case Some(NatAdd(lhs, rhs, _)) =>
                for
                  l <- eval(lhs)
                  r <- eval(rhs)
                yield l + r
              case Some(NatMul(lhs, rhs, _)) =>
                for
                  l <- eval(lhs)
                  r <- eval(rhs)
                yield l * r
              case Some(d_affine.Min(dimOperands, symbolOperands, map, _)) =>
                val args = dimOperands ++ symbolOperands
                evalAffineApplyConst(args, map, eval)
              case Some(d_affine.Apply(dimOperands, symbolOperands, map, _)) =>
                val args = dimOperands ++ symbolOperands
                evalAffineApplyConst(args, map, eval)
              case Some(arith.Constant(IntegerAttr(IntData(c), _), _)) => Some(c)
              case _                                                    => None
            inProgress -= base
            out
        },
      )

    eval(v)

  def isPositive(v: Value[Attribute]): Boolean =
    val memo = mutable.Map.empty[Value[Attribute], Boolean]
    val inProgress = mutable.Set.empty[Value[Attribute]]

    def eval(x: Value[Attribute]): Boolean =
      val base = resolveNat(x).getOrElse(x)
      memo.getOrElseUpdate(
        base, {
          if inProgress.contains(base) then false
          else
            inProgress += base
            val out =
              base.typ.isInstanceOf[DTensorPosNatType] ||
                exactConst(base).exists(_ > 0) ||
                (base.owner match
                  case Some(ShapeToIndex(nat, _))    => eval(nat)
                  case Some(NatAdd(lhs, rhs, _))     => eval(lhs) || eval(rhs)
                  case Some(NatMul(lhs, rhs, _))     => eval(lhs) && eval(rhs)
                  case Some(d_affine.Apply(dimOperands, symbolOperands, map, _)) =>
                    recoverProjectedNatFromApply(dimOperands, symbolOperands, map).exists(eval)
                  case Some(arith.Constant(IntegerAttr(IntData(c), _), _)) => c > 0
                  case _                                                    => false
                )
            inProgress -= base
            out
        },
      )

    eval(v)
