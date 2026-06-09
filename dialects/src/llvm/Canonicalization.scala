package scair.dialects.llvm.canonicalization

import scair.dialects.builtin.*
import scair.dialects.llvm.*
import scair.ir.*
import scair.transformations.*
import scair.transformations.CanonicalizationPatterns

val AddFold = pattern {
  case Add(lhs = x, rhs = Owner(Constant(IntegerAttr(IntData(0), _), _)), overflowFlags = None) =>
    (Seq(), Seq(x))
  case Add(lhs = Owner(Constant(IntegerAttr(IntData(0), _), _)), rhs = x, overflowFlags = None) =>
    (Seq(), Seq(x))
  case Add(
        lhs = Owner(Constant(c0: IntegerAttr, _)),
        rhs = Owner(Constant(c1: IntegerAttr, _)),
        overflowFlags = None,
      ) =>
    Constant(c0 + c1, Result(c0.typ))
}

given CanonicalizationPatterns[Add](AddFold)
