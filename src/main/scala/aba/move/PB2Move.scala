package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}


object PB2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    // case a)
    val culpritsCandidatesWithoutP = framework.culpritsCandidates -- dState.pLitArgs.map(_.lit)
    val caseA = framework.remainingNonBlockedPRules.filter( rule => culpritsCandidatesWithoutP.contains(rule.head)).map(RuleArgument) // definition
      .diff(dState.pRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id) // sorting
      .map(
      ruleArg =>
        PotentialMove(
          Some(ruleArg),
          None,
          ruleArg.rule.body.map(LiteralArgument) + LiteralArgument(ruleArg.rule.head),
          Move.PB2,
          Some("case (a)"))
    )

    // case b)
    val caseB = ((framework.assumptions intersect framework.contrariesOf(framework.culpritsCandidates)) --
      (dState.pLitArgs.map(_.lit) ++ framework.blockedAssumptionsP)).map(LiteralArgument) // definition
      .diff(dState.pLitArgs) // prevent from repeating
      .toSeq.sortBy(_.lit.id) // sorting
      .map(
        litArg =>
          PotentialMove(
            None,
            Some(litArg),
            Set(litArg),
            Move.PB2,
            Some("case (b)")
          )
    )
      caseA ++ caseB
  }
}
