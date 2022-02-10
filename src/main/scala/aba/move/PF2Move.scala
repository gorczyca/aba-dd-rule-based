package aba.move

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.{DC, DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, PF2}
import aba.reasoner.{DisputeState, PotentialAssumptionMove, PotentialMove2}

object PF2Move extends Move {
  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    // possible assumptions
    val possibleAssumptions = ((advancementType match {
      case DC => dState.culpritCandidatesContraries ++ dState.currentlyDefendedAssumptions
      case DF => framework.assumptions
      case _ => dState.culpritCandidatesContraries
    }) intersect framework.assumptions) diff (dState.pStatements union framework.selfContradictingAssumptions union dState.culprits union dState.defenceContraries) // TODO: instead of pStatements it should be enough to just do defences

    possibleAssumptions.map(ass => {

      val attackedByAssumption = framework.contraries.filter(_.contrary == ass).map(_.assumption)
      // TODO: more efficient
      val attackedAssumptions = (advancementType match {
        case DF => framework.assumptions
        case _ => dState.culpritCandidates
      }) intersect attackedByAssumption

     PF2Move(ass, Some(attackedAssumptions))
    }).toSeq

  }
}

case class PF2Move(override val assumption: String,
                   override val attacking: Option[Set[String]],
                   override val moveType: MoveType = PF2
                  ) extends PotentialAssumptionMove(assumption) { // TODO: cleaner

  override def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

    val newBStatements = dState.bStatements + assumption

    val attackedByAssumption = framework.contraries.filter(_.contrary == assumption).map(_.assumption)
    val newCulprits = dState.culprits ++ attackedByAssumption
    val newDefences = dState.defences + assumption

    // TODO: again, only if attacked by assumption is not empty
    val newPRemainingNonBlockedRules = dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, body) => (body intersect attackedByAssumption).isEmpty
    }

    // TODO: same here as above
    val newBRemainingNonBlockedRules = dState.pRemainingNonBlockedRules.filter {
      case Rule(_, _, body) => (body intersect attackedByAssumption).isEmpty
    }
    // TODO: same
    val newBFullyExpandedStatements = newBStatements.filter(st => !newBRemainingNonBlockedRules.exists(rule => rule.head == st))  // TODO: here optimization possible

    // TODO: same
    val (newBPlayedBlockedStatements, newBPlayedBlockedRules) = dState.calculateBPlayedBlockedPiecesRec(newCulprits, Set.empty, newBFullyExpandedStatements, dState.bRules)

    // unblocked complete pieces - TODO: again only if new culprits appear
    val initialCompleteUnblockedStatements = (newBStatements intersect framework.assumptions) -- newCulprits
    val remainingNonBlockedStatements = newBStatements -- newBPlayedBlockedStatements
    val remainingNonBlockedRules = dState.bRules  -- newBPlayedBlockedRules
    val (newBUnblockedCompleteStatements, newBUnblockedCompleteRules) = dState.calculateBUnblockedCompletePiecesRec(initialCompleteUnblockedStatements, dState.bRules, remainingNonBlockedStatements -- framework.assumptions, remainingNonBlockedRules)

    // TODO: all of the above only changes then

    // supporting defence contraries // TODO: this not actually
    val initialStatementsSupportingDefenceContraries = remainingNonBlockedStatements intersect framework.contrariesOf(newDefences)
    val (newBUnblockedStatementsSupportingDefenceContraries, newBUnblockedRulesSupportingDefenceContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefenceContraries, Set.empty, remainingNonBlockedStatements, remainingNonBlockedRules)


    // supporting defended assumptions contraries
    val attackedByBUnblockedCompleteStatements = framework.contraries.filter(ctr => newBUnblockedCompleteStatements.contains(ctr.contrary)).map(_.assumption)
    val newCurrentlyDefendedAssumptions = (framework.assumptions -- newCulprits) -- attackedByBUnblockedCompleteStatements

    val newCurrentlyDefendedAssumptionsContraries = framework.contrariesOf(newCurrentlyDefendedAssumptions)


    val initialStatementsSupportingDefendedAssumptionsContraries = remainingNonBlockedStatements intersect framework.contrariesOf(newCurrentlyDefendedAssumptions)
    val (newBUnblockedStatementsSupportingDefendedAssumptionsContraries, newBUnblockedRulesSupportingDefendedAssumptionsContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefendedAssumptionsContraries, Set.empty, remainingNonBlockedStatements, remainingNonBlockedRules)

    // TODO: again: this only changes
    val newCulpritCandidates = newBUnblockedStatementsSupportingDefenceContraries intersect framework.assumptions

    val newDefenceContraries = dState.defenceContraries ++ framework.contrariesOf(assumption)
    val newCulpritCandidatesContraries = framework.contrariesOf(newCulpritCandidates)

    DisputeState(
      pStatements = dState.pStatements + assumption,
      pRules = dState.pRules,
      oStatements = dState.oStatements,
      oRules = dState.oRules,
      defences = dState.defences + assumption,
      culprits = newCulprits,
      pRemainingNonBlockedRules = newPRemainingNonBlockedRules,
      bRemainingNonBlockedRules = newBRemainingNonBlockedRules,
      pPlayedUnexpandedStatements = dState.pPlayedUnexpandedStatements,
      bFullyExpandedStatements = newBFullyExpandedStatements,
      bPlayedBlockedStatements = newBPlayedBlockedStatements,
      bPlayedBlockedRules = newBPlayedBlockedRules,
      pPlayedCompleteStatements = dState.pPlayedCompleteStatements, // TODO: this should not change
      pPlayedCompleteRules = dState.pPlayedCompleteRules,
      bUnblockedCompletePlayedStatements = newBUnblockedCompleteStatements,
      bUnblockedCompletePlayedRules = newBUnblockedCompleteRules,
      bUnblockedStatementsSupportingDefenceContraries = newBUnblockedStatementsSupportingDefenceContraries,
      bUnblockedRulesSupportingDefenceContraries = newBUnblockedRulesSupportingDefenceContraries,
      bUnblockedStatementsSupportingDefendedAssumptionsContraries = newBUnblockedStatementsSupportingDefendedAssumptionsContraries,
      bUnblockedRulesSupportingDefendedAssumptionsContraries = newBUnblockedRulesSupportingDefendedAssumptionsContraries,
      culpritCandidates = newCulpritCandidates,
      currentlyDefendedAssumptions = newCurrentlyDefendedAssumptions,
      defenceContraries = newDefenceContraries,
      culpritCandidatesContraries = newCulpritCandidatesContraries,
      currentlyDefendedAssumptionsContraries = newCurrentlyDefendedAssumptionsContraries
    )
  }
}