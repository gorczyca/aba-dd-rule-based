package aba.move

import aba.framework.{Contrary, Framework, Rule}
import aba.move.DisputeAdvancement.{DC, DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OB2}
import aba.reasoner.{DisputeState, OpponentMove, PotentialMove2, PotentialRuleMove}

object OB2Move extends Move with OpponentsAttackingMove {

  def validRuleHeads(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Set[String] = {
    advancementType match {
      case DC => dState.defenceContraries ++ dState.currentlyDefendedAssumptionsContraries
      case DF => framework.contraries.map(_.contrary) // contraries of any defence
      case _ => dState.defenceContraries
    }
  }

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {
    val ruleHeads = validRuleHeads(advancementType)

    dState.bRemainingNonBlockedRules.filter {
      case Rule(_, head, _) => ruleHeads.contains(head)
    }.map(rule => OB2Move(rule, attackedAssumptionsBySet(rule.statements, advancementType))).toSeq
  }
}

// TODO: make traits such as OpponentMove, RuleMove, ProponentMove, AssumptionMove, AttackingMove etc
case class OB2Move(override val rule: Rule,
                   override val attacking: Option[Set[String]],
                  ) extends PotentialRuleMove with OpponentMove {

  override val moveType: MoveType = OB2

}
