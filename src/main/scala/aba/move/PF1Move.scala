package aba.move

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, PF1}
import aba.reasoner.{DisputeState, NonAttackingMove, PotentialMove2, PotentialRuleMove, ProponentMove}

object PF1Move extends Move with ProponentsAttackingMove {

  private def validHeads(implicit framework: Framework, dState: DisputeState): Set[String] = {
    (framework.alphabet diff dState.pStatements) union dState.pPlayedUnexpandedStatements
  }

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val heads = validHeads

    dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, body) => heads.contains(head) && body.subsetOf(dState.pPlayedCompleteStatements)
    }.map(rule => PF1Move(rule, attackedAssumptionsBySet(rule.statements, advancementType))
    ).toSeq
  }
}

case class PF1Move(override val rule: Rule,
                   override val attacking: Option[Set[String]]) extends PotentialRuleMove with ProponentMove {

  override val moveType: MoveType = PF1

}