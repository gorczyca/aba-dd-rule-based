package aba.move

import aba.framework.{Contrary, Framework, Rule}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, PB1, possibleMovesAccordingToAllAdvancementToString}
import aba.reasoner.{DisputeState, NonAttackingMove, PotentialMove2, PotentialRuleMove, ProponentMove}

object PB1Move extends Move with ProponentsAttackingMove {

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, _) => dState.pPlayedUnexpandedStatements.contains(head)
    }.map(rule => PB1Move(rule, attackedAssumptionsBySet(rule.statements, advancementType))).toSeq
  }
}


case class PB1Move(override val rule: Rule, override val attacking: Option[Set[String]]) extends PotentialRuleMove with ProponentMove {
  override val moveType: MoveType = PB1

}