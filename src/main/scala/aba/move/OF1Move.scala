package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OF1Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    val unblockedCompletePlayedPiecesLit = framework.unblockedCompletePlayedPiecesB.collect { case litArg: LiteralArgument => litArg }.map(_.lit)

    framework.remainingNonBlockedBRules.filter(_.body.subsetOf(unblockedCompletePlayedPiecesLit)).map(RuleArgument) // definition
      .diff(dState.bRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id)
      .map(
        ruleArg => PotentialMove(Some(ruleArg), None, Set(LiteralArgument(ruleArg.rule.head)), Move.OF1, None)
    )
  }
}
