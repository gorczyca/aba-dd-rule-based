package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OB1Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    framework.remainingNonBlockedBRules.map(RuleArgument).filter(_.children.intersect(dState.b).nonEmpty).map(
      ruleArg => PotentialMove(Some(ruleArg), ruleArg.rule.body.map(LiteralArgument), Move.OB1, None)
    )
  }
}
