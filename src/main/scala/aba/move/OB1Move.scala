package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OB1Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    framework.remainingNonBlockedBRules.map(RuleArgument).filter(_.children.intersect(dState.b).nonEmpty) // by definition
      .diff(dState.bRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id)
      .map(ruleArg => PotentialMove(Some(ruleArg), None, ruleArg.rule.body.map(LiteralArgument), Move.OB1, None))
  }
}
