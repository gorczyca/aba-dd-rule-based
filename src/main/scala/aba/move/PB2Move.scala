package aba.move

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.{DisputeAdvancementType, DF}
import aba.move.Move.{MoveType, PB2}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, PotentialMove2, PotentialRuleMove, RuleArgument}


object PB2Move extends Move {
  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val validHeads = (advancementType match {
      case DF => framework.contraries.map(_.contrary)
      case _ => dState.culpritCandidatesContraries
    }) diff (dState.pStatements ++ dState.defenceContraries) // TODO: IF necessary remove pStatements from here too. But it should already dealt with ???

    dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, _) => validHeads.contains(head)
    }.map(rule => {
      // TODO: optimize

      val ruleIsAttacking = framework.contraries.filter(_.contrary == rule.head).map(_.assumption)

      val attackedAssumptions = (advancementType match {
        case DF => framework.assumptions
        case _ => dState.culpritCandidates
      }) intersect ruleIsAttacking

      PB2Move(rule, Some(attackedAssumptions))
    }).toSeq
  }
}


case class PB2Move(override val rule: Rule,
                   override val attacking: Option[Set[String]],
                   override val moveType: MoveType = PB2
                  ) extends PotentialRuleMove(rule) {

  override def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

    val newBStatements = dState.bStatements ++ rule.statements
    val newBRules = dState.bRules + rule

    // TODO: same as before PB1
    val ruleAssumptions = rule.body intersect framework.assumptions
    val newDefences = dState.defences ++ ruleAssumptions
    val attackedByRule = framework.contraries.filter(ctr => rule.statements.contains(ctr.contrary)).map(_.assumption)
    val newCulprits = dState.culprits ++ (attackedByRule intersect framework.assumptions)

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

    // B unblocked pieces
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
      pStatements = dState.pStatements ++ rule.statements,
      pRules = dState.pRules + rule,
      oStatements = dState.oStatements,
      oRules = dState.oRules,
      defences = newDefences,
      culprits = newCulprits,
      pRemainingNonBlockedRules = newPRemainingNonBlockedRules,
      bRemainingNonBlockedRules = newBRemainingNonBlockedRules,
      pPlayedUnexpandedStatements = dState.pPlayedUnexpandedStatements - rule.head,
      bFullyExpandedStatements = newBFullyExpandedStatements,
      bPlayedBlockedStatements = newBPlayedBlockedStatements, // TODO: maybe these do not need to be in the dispute state
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