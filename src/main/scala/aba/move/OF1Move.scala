package aba.move

import aba.framework.{Framework, Literal, Rule}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OF1}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, PotentialMove2, PotentialRuleMove, RuleArgument}

object OF1Move extends Move {
  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    dState.bRemainingNonBlockedRules.filter{
      case Rule(_, _, body) => body.subsetOf(dState.bUnblockedCompletePlayedStatements)
    }.map(rule => {
      // TODO: again, check if I'm attacking sth here?
      OF1Move(rule, None)
    }).toSeq
  }
}

case class OF1Move(override val rule: Rule,
                   override val attacking: Option[Set[String]],
                   override val moveType: MoveType = OF1
                  ) extends PotentialRuleMove(rule) {

  override def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

    val newBStatements = dState.bStatements + rule.head
    val newBRules = dState.bRules + rule

    val newBRemainingNonBlockedRules = dState.bRemainingNonBlockedRules - rule
    val newBFullyExpandedStatements = newBStatements.filter(st => !newBRemainingNonBlockedRules.exists(rule => rule.head == st))  // TODO: here optimization possible

    // played blocked pieces
    val (newBPlayedBlockedStatements, newBPlayedBlockedRules) = dState.calculateBPlayedBlockedPiecesRec(dState.culprits, Set.empty, newBFullyExpandedStatements -- framework.assumptions, newBRules)

    // unblocked complete pieces - TODO: since no new culprits appear, we can reuse what we have already
    val initialCompleteUnblockedStatements = dState.bUnblockedCompletePlayedStatements // ++ ruleAssumptions // because we are using this rule we are sure it has no culprits // TODO: do we even need the assumptions?
    val remainingNonBlockedStatements = (newBStatements -- newBPlayedBlockedStatements) -- initialCompleteUnblockedStatements // TODO
    val remainingNonBlockedRules = (newBRules -- newBPlayedBlockedRules) -- dState.bUnblockedCompletePlayedRules
    val (newBUnblockedCompleteStatements, newBUnblockedCompleteRules) =
      dState.calculateBUnblockedCompletePiecesRec(initialCompleteUnblockedStatements, dState.bUnblockedCompletePlayedRules, remainingNonBlockedStatements -- framework.assumptions, remainingNonBlockedRules)

    // unblocked pieces supporting defence contraries // TODO: this probably can be optimized, reuse what we had!
    val initialStatementsSupportingDefenceContraries = remainingNonBlockedStatements intersect framework.contrariesOf(dState.defences)
    val remainingNonBlockedStatementsPossiblySupportingDefenceContraries = newBStatements -- newBPlayedBlockedStatements
    val remainingNonBlockedRulesPossiblySupportingDefenceContraries = newBRules -- newBPlayedBlockedRules
    val (newBUnblockedStatementsSupportingDefenceContraries, newBUnblockedRulesSupportingDefenceContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefenceContraries, Set.empty, remainingNonBlockedStatementsPossiblySupportingDefenceContraries, remainingNonBlockedRulesPossiblySupportingDefenceContraries)

    val attackedByBUnblockedCompleteStatements = framework.contraries.filter(ctr => newBUnblockedCompleteStatements.contains(ctr.contrary)).map(_.assumption)
    val newCurrentlyDefendedAssumptions = framework.assumptions -- attackedByBUnblockedCompleteStatements
    val newCurrentlyDefendedAssumptionsContraries = framework.contrariesOf(newCurrentlyDefendedAssumptions)


    // unblocked pieces supporting contraries of curr def assumptions // TODO: this probably can be optimized, reuse what we had!
    val initialStatementsSupportingContrariesOfCurrentlyDefendedAssumptions = remainingNonBlockedStatements intersect newCurrentlyDefendedAssumptionsContraries
    val (newBUnblockedStatementsSupportingContrariesOfCurrentlyDefendedAssumptions, newBUnblockedRulesSupportingContrariesOfCurrentlyDefendedAssumptions) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingContrariesOfCurrentlyDefendedAssumptions, Set.empty, remainingNonBlockedStatementsPossiblySupportingDefenceContraries, remainingNonBlockedRulesPossiblySupportingDefenceContraries)

    val newCulpritCandidates = framework.assumptions intersect newBUnblockedStatementsSupportingDefenceContraries
    val newCulpritCandidatesContraries = framework.contrariesOf(newCulpritCandidates)

    DisputeState(
      pStatements = dState.pStatements,
      pRules = dState.pRules,
      oStatements = dState.oStatements ++ rule.body,
      oRules = dState.oRules + rule,
      defences = dState.defences,
      culprits = dState.culprits,
      pRemainingNonBlockedRules = dState.pRemainingNonBlockedRules,
      bRemainingNonBlockedRules = newBRemainingNonBlockedRules,
      pPlayedUnexpandedStatements = dState.pPlayedCompleteStatements,
      bFullyExpandedStatements = newBFullyExpandedStatements,
      bPlayedBlockedStatements = newBPlayedBlockedStatements, // TODO: This should not change
      bPlayedBlockedRules = newBPlayedBlockedRules,           // TODO: this as well
      pPlayedCompleteStatements = dState.pPlayedCompleteStatements,
      pPlayedCompleteRules = dState.pPlayedCompleteRules,
      bUnblockedCompletePlayedStatements = newBUnblockedCompleteStatements,
      bUnblockedCompletePlayedRules = newBUnblockedCompleteRules,
      bUnblockedStatementsSupportingDefenceContraries = newBUnblockedStatementsSupportingDefenceContraries,
      bUnblockedRulesSupportingDefenceContraries = newBUnblockedRulesSupportingDefenceContraries,
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
