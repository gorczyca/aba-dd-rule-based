package aba.move

import aba.framework.Framework
import aba.move.DisputeAdvancement.{DC, DF, DS, DisputeAdvancementType}
import aba.move.Move.{MoveType, PF2}
import aba.reasoner.{DisputeState, PotentialAssumptionMove, PotentialMove2, ProponentMove}

object PF2Move extends Move with ProponentsAttackingMove {

  private def possibleAssumptions(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Set[String] = {

    val possibleAssumptions = ((advancementType match {
      case DC => dState.culpritCandidatesContraries ++ dState.currentlyDefendedAssumptions
      case DF | DS  => framework.assumptions
      case _ => dState.culpritCandidatesContraries
    }) intersect framework.assumptions)

    val nonBlockedAssumptions = possibleAssumptions diff (
      dState.pStatements union
        framework.selfContradictingAssumptions union
        dState.culprits union
        dState.defenceContraries union
        framework.constraints) // TODO: instead of pStatements it should be enough to just do defences

    nonBlockedAssumptions
  }

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    // possible assumptions
    val assumptions = possibleAssumptions(advancementType)

    assumptions.map(ass =>
      PF2Move(ass, attackedAssumptionsBySet(Set(ass), advancementType))).toSeq
  }
}

case class PF2Move(override val assumption: String,
                   override val attacking: Option[Set[String]],
                  ) extends PotentialAssumptionMove with ProponentMove { // TODO: cleaner

  override val moveType: MoveType = PF2

}