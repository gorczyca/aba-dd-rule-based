package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object PB1Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    val childArgs = framework.unexpandedPStatements -- framework.completePiecesP
    // map rules to potential rule arguments to access the "pChildren" method
    framework.remainingNonBlockedPRules.map(RuleArgument).filter(_.pChildren.intersect(childArgs).nonEmpty) // definition
      .diff(dState.pRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id)
      .map( ruleArg => PotentialMove(Some(ruleArg), None, ruleArg.rule.body.map(LiteralArgument), Move.PB1, None )
    )
  }

}
