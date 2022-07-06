package aba.move

import aba.framework.{Contrary, Framework, Rule}
import aba.move.DisputeAdvancement.{DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, PB2}
import aba.reasoner.{DisputeState, PotentialMove2, PotentialRuleMove, ProponentMove}


object PB2Move extends Move with ProponentsAttackingMove {

  private def validHeads(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Set[String] = {
    (advancementType match {
      case DF => framework.contraries.map(_.contrary)
      case _ => dState.culpritCandidatesContraries
    }) diff (dState.pStatements ++ dState.defenceContraries) // TODO: IF necessary remove pStatements from here too. But it should already dealt with ???
  }

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val heads = validHeads(advancementType)

    dState.pRemainingNonBlockedRules.filter {
      case Rule(_, head, _) => heads.contains(head)
    }.map(rule => PB2Move(rule, attackedAssumptionsBySet(rule.statements, advancementType))).toSeq
  }
}


case class PB2Move(override val rule: Rule,
                   override val attacking: Option[Set[String]],
                  ) extends PotentialRuleMove with ProponentMove {

  override val moveType: MoveType = PB2

}