package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}


object PB2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    // case a)
    val culpritsCandidatesWithoutP = framework.culpritsCandidates -- dState.pLitArgs.map(_.lit)
    val caseA = framework.remainingNonBlockedPRules.filter( rule => culpritsCandidatesWithoutP.contains(rule.head)).map(
      rule =>
        PotentialMove(
          Some(RuleArgument(rule)),
          rule.body.map(LiteralArgument) + LiteralArgument(rule.head),
          Move.PB2,
          Some("case (a)"))
    )

    // case b)
    val caseB = ((framework.assumptions intersect framework.contrariesOf(framework.culpritsCandidates)) --
      (dState.pLitArgs.map(_.lit) ++ framework.blockedAssumptionsP)).map(
        lit =>
          PotentialMove(
            None,
            Set(LiteralArgument(lit)),
            Move.PB2,
            Some("case (b)")
          )
    )
      caseA ++ caseB
  }
}
