package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OB2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    // case a)
    val defencesContraries = framework.contrariesOf(framework.defences)
    val caseA = framework.remainingNonBlockedBRules.filter(rule => defencesContraries.contains(rule.head)).map(
      rule => PotentialMove(
        Some(RuleArgument(rule)),
        rule.body.map(LiteralArgument) + LiteralArgument(rule.head),
        Move.OB2,
        Some("case a)")
      )
    )

    // case b)
    val caseB = (defencesContraries.intersect(framework.assumptions) -- (framework.culprits ++ framework.defences)).map(
      lit => PotentialMove(None, Set(LiteralArgument(lit)), Move.OB2, Some("case b)"))
    )

    caseA ++ caseB
  }
}
