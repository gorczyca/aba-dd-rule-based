package aba.move

import aba.framework.Framework
import aba.move.DisputeAdvancement.{DC, DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OF2}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialAssumptionMove, PotentialMove, PotentialMove2, PotentialRuleMove}

object OF2Move extends Move {
  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val possibleAssumptions = (advancementType match {
      case DC => dState.defenceContraries ++ dState.currentlyDefendedAssumptionsContraries
      case DF => framework.assumptions
      case _ => dState.defenceContraries
    }) intersect framework.assumptions

    possibleAssumptions.map(ass => {
      // TODO: again here, have some map of attacks in order to further optimize

      val assIsContraryOf = framework.contraries.filter(_.contrary == ass).map(_.assumption)

      val attackedAssumptions = (advancementType match {
        case DC => dState.defences ++ dState.currentlyDefendedAssumptions
        case DF => framework.assumptions // TODO: here indicate only defences or any assumptions?
        case _ => dState.defences
      }) intersect assIsContraryOf

      OF2Move(ass, Some(attackedAssumptions))

    }).toSeq
  }
}

case class OF2Move(override val assumption: String,
                   override val attacking: Option[Set[String]],
                   override val moveType: MoveType = OF2
                  ) extends PotentialAssumptionMove(assumption) {

  override def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

    val newBStatements = dState.bStatements + assumption

    val newBFullyExpandedStatements = dState.bFullyExpandedStatements + assumption

    val newBUnblockedCompleteStatements = dState.bFullyExpandedStatements + assumption

    val attackedByAssumption = framework.contraries.filter(_.contrary == assumption).map(_.assumption)

    val newBUnblockedStatementsSupportingDefenceContraries =
      if ((dState.defences intersect attackedByAssumption).nonEmpty) dState.bUnblockedStatementsSupportingDefenceContraries + assumption
      else dState.bUnblockedStatementsSupportingDefenceContraries

    val newCurrentlyDefendedAssumptions = dState.currentlyDefendedAssumptions -- attackedByAssumption

    val newCurrentlyDefendedAssumptionsContraries = framework.contrariesOf(newCurrentlyDefendedAssumptions)

    // unblocked pieces supporting contraries of curr def assumptions // TODO: this probably can be optimized, reuse what we had!
    val remainingNonBlockedStatements = newBStatements -- dState.bPlayedBlockedStatements
    val remainingNonBlockedRules = dState.bRules -- dState.bPlayedBlockedRules
    val initialStatementsSupportingContrariesOfCurrentlyDefendedAssumptions =  remainingNonBlockedStatements intersect newCurrentlyDefendedAssumptionsContraries

    val (newBUnblockedStatementsSupportingContrariesOfCurrentlyDefendedAssumptions, newBUnblockedRulesSupportingContrariesOfCurrentlyDefendedAssumptions) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingContrariesOfCurrentlyDefendedAssumptions, Set.empty, remainingNonBlockedStatements -- initialStatementsSupportingContrariesOfCurrentlyDefendedAssumptions, remainingNonBlockedRules)

    val (newCulpritCandidates, newCulpritCandidatesContraries) = if ((dState.defences intersect attackedByAssumption).nonEmpty) {
      val assumptionContraries = framework.contraries.filter(_.assumption == assumption).map(_.contrary)
      (dState.culpritCandidates + assumption, dState.culpritCandidatesContraries ++ assumptionContraries)
    } else (dState.culpritCandidates, dState.culpritCandidatesContraries)


    DisputeState(
      pStatements = dState.pStatements,
      pRules = dState.pRules,
      oStatements = dState.oStatements + assumption,
      oRules = dState.oRules,
      defences = dState.defences,
      culprits = dState.culprits,
      pRemainingNonBlockedRules = dState.pRemainingNonBlockedRules,
      bRemainingNonBlockedRules = dState.bRemainingNonBlockedRules,
      pPlayedUnexpandedStatements = dState.pPlayedCompleteStatements,
      bFullyExpandedStatements = newBFullyExpandedStatements,
      bPlayedBlockedStatements = dState.bPlayedBlockedStatements, // TODO: This should not change
      bPlayedBlockedRules = dState.bPlayedBlockedRules,           // TODO: this as well
      pPlayedCompleteStatements = dState.pPlayedCompleteStatements,
      pPlayedCompleteRules = dState.pPlayedCompleteRules,
      bUnblockedCompletePlayedStatements = newBUnblockedCompleteStatements,
      bUnblockedCompletePlayedRules = dState.bUnblockedCompletePlayedRules,
      bUnblockedStatementsSupportingDefenceContraries = newBUnblockedStatementsSupportingDefenceContraries,
      bUnblockedRulesSupportingDefenceContraries = dState.bUnblockedRulesSupportingDefenceContraries,
      bUnblockedStatementsSupportingDefendedAssumptionsContraries = newBUnblockedStatementsSupportingContrariesOfCurrentlyDefendedAssumptions,
      bUnblockedRulesSupportingDefendedAssumptionsContraries = newBUnblockedRulesSupportingContrariesOfCurrentlyDefendedAssumptions,
      culpritCandidates = newCulpritCandidates,
      currentlyDefendedAssumptions = newCurrentlyDefendedAssumptions,
      defenceContraries = dState.defenceContraries,
      culpritCandidatesContraries = newCulpritCandidatesContraries,
      currentlyDefendedAssumptionsContraries = newCurrentlyDefendedAssumptionsContraries
    )

  }
}
