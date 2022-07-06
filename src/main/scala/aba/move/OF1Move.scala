package aba.move

import aba.framework.{Contrary, Framework, Rule}
import aba.move.DisputeAdvancement.{DC, DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OF1}
import aba.reasoner.{DisputeState, NonAttackingMove, OpponentMove, PotentialMove2, PotentialRuleMove}

object OF1Move extends Move with OpponentsAttackingMove {

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {
    dState.bRemainingNonBlockedRules.filter{
      case Rule(_, _, body) => body.subsetOf(dState.bUnblockedCompletePlayedStatements)
    }.map(rule => OF1Move(rule, attackedAssumptionsBySet(rule.statements, advancementType))).toSeq
  }
}

case class OF1Move(override val rule: Rule,
                   override val attacking: Option[Set[String]]) extends PotentialRuleMove with OpponentMove {

  override val moveType: MoveType = OF1
}
