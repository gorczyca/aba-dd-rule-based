package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object PB1Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {

    framework.remainingNonBlockedPRules.filter(rule => framework.unexpandedPStatements.contains(rule.head))
      .map(RuleArgument)
      .diff(dState.pRuleArgs)
      .toSeq.sortBy(_.rule.head.id)
      .map( ruleArg => PotentialMove(Some(ruleArg), None, ruleArg.rule.body.map(LiteralArgument), Move.PB1, None ))

  }
}
