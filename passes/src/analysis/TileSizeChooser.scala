package scair.passes.analysis

import scair.ir.*
import scair.passes.NatProvenance

object TileSizeChooser:
  val DefaultCandidates: Seq[Int] = Seq(4, 8, 16, 32, 64)

  def chooseLargestGuaranteed(
      facts: NatDivisibilityFacts,
      dim: Value[Attribute],
      candidates: Seq[Int] = DefaultCandidates,
  ): Option[Int] =
    facts.largestDivisibleIn(dim, candidates)

  def chooseLargestGuaranteedFromProvenance(
      facts: NatDivisibilityFacts,
      dim: Value[Attribute],
      candidates: Seq[Int] = DefaultCandidates,
  ): Option[Int] =
    NatProvenance.resolveNat(dim) match
      case Some(nat) => chooseLargestGuaranteed(facts, nat, candidates)
      case None      => chooseLargestGuaranteed(facts, dim, candidates)
