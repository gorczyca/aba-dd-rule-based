package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object PF1Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    val completePiecesPLit = framework.completePiecesP.collect { case litArg: LiteralArgument => litArg.lit }

    framework.remainingNonBlockedPRules
      .filter( rule => (!dState.pLitArgs.map(_.lit).contains(rule.head) || framework.unexpandedPStatements.contains(rule.head))
        && rule.body.subsetOf(completePiecesPLit) ) // definition
      .map(RuleArgument)
      .diff(dState.pRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id) // sorting
      .map( ruleArg =>
        PotentialMove(Some(ruleArg), None, Set(LiteralArgument(ruleArg.rule.head)), Move.PF1, None) )
  }
}
