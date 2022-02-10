package aba.move

import aba.framework.{Framework, Literal, Rule}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, PF1}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, PotentialMove2, PotentialRuleMove, RuleArgument}

object PF1Move extends Move {
  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val validHeads = (framework.alphabet diff dState.pStatements) union dState.pPlayedUnexpandedStatements

    dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, body) => validHeads.contains(head) && body.subsetOf(dState.pPlayedCompleteStatements)
    }.map(rule =>
      // TODO: check if attacking should be here
      PF1Move(rule, None)
    ).toSeq
  }
}

case class PF1Move(override val rule: Rule,
                   override val attacking: Option[Set[String]],
                   override val moveType: MoveType = PF1
                  ) extends PotentialRuleMove(rule) {

  override def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

    val newBStatements = dState.bStatements + rule.head
    val newBRules = dState.bRules + rule

    val attackedByRule = framework.contraries.filter(_.contrary == rule.head).map(_.assumption)
    val newCulprits = dState.culprits ++ attackedByRule

    // TODO: to further optimize we could maybe check first if not empty
    val newPRemainingNonBlockedRules = dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, body) => head != rule.head && (body intersect attackedByRule).isEmpty
    }

    val newBRemainingNonBlockedRules = dState.bRemainingNonBlockedRules.filter {
      case Rule(_, _, body) => (body intersect attackedByRule).isEmpty
    }

    val newBFullyExpandedStatements = newBStatements.filter(st => !newBRemainingNonBlockedRules.exists(rule => rule.head == st))  // TODO: here optimization possible

    // TODO: here optimization possible too. Do it only if there are new culprits!!!
    val (newBPlayedBlockedStatements, newBPlayedBlockedRules) = dState.calculateBPlayedBlockedPiecesRec(newCulprits, Set.empty, newBFullyExpandedStatements, newBRules)

    val newRemainingRules = (dState.pRules + rule) -- dState.pPlayedCompleteRules
    val (newPCompleteStatements, newPCompleteRules) = dState.calculatePPlayedCompletePiecesRec(dState.pPlayedCompleteStatements, dState.pPlayedCompleteRules, newRemainingRules)

    // unblocked complete pieces - TODO: again only if new culprits appear
    val initialCompleteUnblockedStatements = (newBStatements intersect framework.assumptions) -- newCulprits
    val remainingNonBlockedStatements = newBStatements -- newBPlayedBlockedStatements
    val remainingNonBlockedRules = newBRules  -- newBPlayedBlockedRules
    val (newBUnblockedCompleteStatements, newBUnblockedCompleteRules) = dState.calculateBUnblockedCompletePiecesRec(initialCompleteUnblockedStatements, newBRules, remainingNonBlockedStatements -- framework.assumptions, remainingNonBlockedRules)

    // supporting defence contraries
    val initialStatementsSupportingDefenceContraries = remainingNonBlockedStatements intersect framework.contrariesOf(dState.defences)
    val (newBUnblockedStatementsSupportingDefenceContraries, newBUnblockedRulesSupportingDefenceContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefenceContraries, Set.empty, remainingNonBlockedStatements, remainingNonBlockedRules)

    // supporting defended assumptions contraries
    val attackedByBUnblockedCompleteStatements = framework.contraries.filter(ctr => newBUnblockedCompleteStatements.contains(ctr.contrary)).map(_.assumption)
    val newCurrentlyDefendedAssumptions = (framework.assumptions -- newCulprits) -- attackedByBUnblockedCompleteStatements
    val newCurrentlyDefendedAssumptionsContraries = framework.contrariesOf(newCurrentlyDefendedAssumptions)


    val initialStatementsSupportingDefendedAssumptionsContraries = remainingNonBlockedStatements intersect framework.contrariesOf(newCurrentlyDefendedAssumptions)
    val (newBUnblockedStatementsSupportingDefendedAssumptionsContraries, newBUnblockedRulesSupportingDefendedAssumptionsContraries) = dState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingDefendedAssumptionsContraries, Set.empty, remainingNonBlockedStatements, remainingNonBlockedRules)

    // TODO: all of that only if new attacks! optimize
    val newCulpritCandidates = newBUnblockedStatementsSupportingDefenceContraries intersect framework.assumptions

    val newCulpritCandidatesContraries = framework.contrariesOf(newCulpritCandidates)

    DisputeState(
      pStatements = dState.pStatements + rule.head,
      pRules = dState.pRules + rule,
      oStatements = dState.oStatements,
      oRules = dState.oRules,
      defences = dState.defences,
      culprits = newCulprits,
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
      defenceContraries = dState.defenceContraries,
      culpritCandidatesContraries = newCulpritCandidatesContraries,
      currentlyDefendedAssumptionsContraries = newCurrentlyDefendedAssumptionsContraries
    )

  }

}