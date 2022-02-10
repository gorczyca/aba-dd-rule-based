package aba.move

import aba.framework.{Contrary, Framework, Rule}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, PB1, possibleMovesAccordingToAllAdvancementToString}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, PotentialMove2, PotentialRuleMove, RuleArgument}

object PB1Move extends Move {
  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, _) =>dState.pPlayedUnexpandedStatements.contains(head)
    }.map(rule => {
      // TODO: if attacking at the same time
      PB1Move(rule, None)
    }).toSeq
  }
}


case class PB1Move(override val rule: Rule,
                   override val attacking: Option[Set[String]], // TODO: if not necessary then set default to None
                   override val moveType: MoveType = PB1
                  ) extends PotentialRuleMove(rule) {

  override def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

    // helpers
    // TODO: move them somewhere
    val ruleAssumptions = rule.body intersect framework.assumptions
    val attackedByRule = framework.contraries.filter(ctr => rule.statements.contains(ctr.contrary)).map(_.assumption)
    val newCulprits = dState.culprits ++ (attackedByRule intersect framework.assumptions)
    val newDefences = dState.defences ++ ruleAssumptions
    val newBRules = dState.bRules + rule
    val newBStatements = dState.bStatements ++ rule.body

    val newPRemainingNonBlockedRules = dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, body) => head != rule.head && (body intersect attackedByRule).isEmpty
    }

    val newBRemainingNonBlockedRules = dState.bRemainingNonBlockedRules.filter {
      case Rule(_, _, body) => (body intersect attackedByRule).isEmpty
    }

    val newBFullyExpandedStatements = newBStatements.filter(st => !newBRemainingNonBlockedRules.exists(rule => rule.head == st))  // TODO: here optimization possible

    val (newBPlayedBlockedStatements, newBPlayedBlockedRules) = dState.calculateBPlayedBlockedPiecesRec(newCulprits, Set.empty, newBFullyExpandedStatements, newBRules)

    // this should be monotonic
    val newRemainingRules = (dState.pRules + rule) -- dState.pPlayedCompleteRules
    val (newPCompleteStatements, newPCompleteRules) = dState.calculatePPlayedCompletePiecesRec(dState.pPlayedCompleteStatements, dState.pPlayedCompleteRules, newRemainingRules)

    // unblocked complete pieces - TODO: again only if new culprits appear
    val initialCompleteUnblockedStatements = (newBStatements intersect framework.assumptions) -- newCulprits
    val remainingNonBlockedStatements = newBStatements -- newBPlayedBlockedStatements
    val remainingNonBlockedRules = newBRules  -- newBPlayedBlockedRules
    val (newBUnblockedCompleteStatements, newBUnblockedCompleteRules) = dState.calculateBUnblockedCompletePiecesRec(initialCompleteUnblockedStatements, newBRules, remainingNonBlockedStatements -- framework.assumptions, remainingNonBlockedRules)

    // supporting defence contraries
    val initialStatementsSupportingDefenceContraries = remainingNonBlockedStatements intersect framework.contrariesOf(newDefences)
    val (newBUnblockedStatementsSupportingDefenceContraries, newBUnblockedRulesSupportingDefenceContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefenceContraries, Set.empty, remainingNonBlockedStatements, remainingNonBlockedRules)


    // supporting defended assumptions contraries
    val attackedByBUnblockedCompleteStatements = framework.contraries.filter(ctr => newBUnblockedCompleteStatements.contains(ctr.contrary)).map(_.assumption)
    val newCurrentlyDefendedAssumptions = (framework.assumptions -- newCulprits) -- attackedByBUnblockedCompleteStatements
    val newCurrentlyDefendedAssumptionsContraries = framework.contrariesOf(newCurrentlyDefendedAssumptions)

    val initialStatementsSupportingDefendedAssumptionsContraries = remainingNonBlockedStatements intersect framework.contrariesOf(newCurrentlyDefendedAssumptions)
    val (newBUnblockedStatementsSupportingDefendedAssumptionsContraries, newBUnblockedRulesSupportingDefendedAssumptionsContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefendedAssumptionsContraries, Set.empty, remainingNonBlockedStatements, remainingNonBlockedRules)


    val newCulpritCandidates = newBUnblockedStatementsSupportingDefenceContraries intersect framework.assumptions

    val newDefenceContraries = dState.defenceContraries ++ framework.contrariesOf(ruleAssumptions)
    val newCulpritCandidatesContraries = framework.contrariesOf(newCulpritCandidates)

    DisputeState(
      pStatements = dState.pStatements ++ rule.body,
      pRules = dState.pRules + rule,
      oStatements = dState.oStatements,
      oRules = dState.oRules,
      defences = newDefences,
      culprits = dState.culprits,
      pRemainingNonBlockedRules = newPRemainingNonBlockedRules,
      bRemainingNonBlockedRules = newBRemainingNonBlockedRules,
      pPlayedUnexpandedStatements = dState.pPlayedUnexpandedStatements - rule.head,
      bFullyExpandedStatements = newBFullyExpandedStatements,
      bPlayedBlockedStatements = newBPlayedBlockedStatements,
      bPlayedBlockedRules = newBPlayedBlockedRules,
      pPlayedCompleteStatements = newPCompleteStatements,
      pPlayedCompleteRules = newPCompleteRules,
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