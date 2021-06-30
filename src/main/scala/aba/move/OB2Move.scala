package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OB2Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    val union = framework.contrariesOf(framework.defences) union set

    framework.remainingNonBlockedBRules.filter(rule => union.contains(rule.head))
      .map(RuleArgument)
      .diff(dState.bRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id)
      .map(
        ruleArg => PotentialMove(
          Some(ruleArg),
          None,
          ruleArg.rule.body.map(LiteralArgument) + LiteralArgument(ruleArg.rule.head),
          Move.OB2,
          None)
        )
  }
}
