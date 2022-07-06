package aba.move

import aba.framework.{Contrary, Framework, Rule}
import aba.move.DisputeAdvancement.{DC, DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OB1}
import aba.reasoner.{DisputeState, NonAttackingMove, OpponentMove, PotentialMove2, PotentialRuleMove}

object OB1Move extends Move with OpponentsAttackingMove {

  def validRuleHeads(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Set[String] = {
    (advancementType match {
      case DC => dState.bUnblockedStatementsSupportingDefenceContraries ++ dState.bUnblockedStatementsSupportingDefendedAssumptionsContraries
      case DF =>
        val anyContraries = framework.contraries.map(_.contrary)
        val remainingNonBlockedStatements = dState.bStatements -- dState.bPlayedBlockedStatements
        val remainingNonBlockedRules = dState.bRules -- dState.bPlayedBlockedRules
        val initialStatementsSupportingContraries =  remainingNonBlockedStatements intersect anyContraries

        val (newBUnblockedStatementsSupportingContraries, _) = DisputeState.calculateBUnblockedPiecesSupportingStatementsInSet(initialStatementsSupportingContraries, Set.empty, remainingNonBlockedStatements -- initialStatementsSupportingContraries, remainingNonBlockedRules)
        newBUnblockedStatementsSupportingContraries
      // TODO: once there is an implementation, TODO: check if I actually need it or can just use any rules? framework.contraries.map(_.contrary) // contraries of any defence
      case _ => dState.bUnblockedStatementsSupportingDefenceContraries
    }) -- framework.assumptions
  }

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val ruleHeads = validRuleHeads(advancementType)

    dState.bRemainingNonBlockedRules.filter {
      case Rule(_, head, _) => ruleHeads.contains(head)
    }.map(rule => OB1Move(rule, attackedAssumptionsBySet(rule.statements, advancementType))).toSeq
  }
}

case class OB1Move(override val rule: Rule,
                   override val attacking: Option[Set[String]]) extends PotentialRuleMove with OpponentMove {
  override val moveType: MoveType = OB1
}



