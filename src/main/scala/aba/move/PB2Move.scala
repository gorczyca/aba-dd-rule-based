package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}


object PB2Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {

    val setDiff = dState.pLitArgs.map(_.lit) union framework.contrariesOf(framework.defences) union framework.constraints
    val union = framework.contrariesOf(framework.culpritsCandidates) union set
    val possibleRuleHeads = union -- setDiff

    framework.remainingNonBlockedPRules.filter(rule => possibleRuleHeads.contains(rule.head))
      .map(RuleArgument)
      .diff(dState.pRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id) // sorting
      .map(ruleArg =>
        PotentialMove(
          Some(ruleArg),
          None,
          ruleArg.rule.body.map(LiteralArgument) + LiteralArgument(ruleArg.rule.head),
          Move.PB2,
          None)
      )
  }
}
