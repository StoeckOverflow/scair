package scair.passes.analysis

import scair.ir.*
import scair.passes.SizeWitnessProvenance

object TileSizeChooser:
  val DefaultCandidates: Seq[Int] = Seq(4, 8, 16, 32, 64)

  def chooseLargestGuaranteed(
      facts: SizeDivisibilityFacts,
      dim: Value[Attribute],
      candidates: Seq[Int] = DefaultCandidates,
  ): Option[Int] =
    facts.largestDivisibleIn(dim, candidates)

  def chooseLargestGuaranteedFromProvenance(
      facts: SizeDivisibilityFacts,
      dim: Value[Attribute],
      candidates: Seq[Int] = DefaultCandidates,
  ): Option[Int] =
    SizeWitnessProvenance.resolveSizeWitness(dim) match
      case Some(nat) => chooseLargestGuaranteed(facts, nat, candidates)
      case None      => chooseLargestGuaranteed(facts, dim, candidates)
