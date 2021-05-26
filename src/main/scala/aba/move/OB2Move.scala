package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OB2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    // case a)
    val defencesContraries = framework.contrariesOf(framework.defences)
    val caseA = framework.remainingNonBlockedBRules.filter(rule => defencesContraries.contains(rule.head)).map(RuleArgument) // by definition
      .diff(dState.bRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id) // sorting
      .map(
        ruleArg => PotentialMove(
          Some(ruleArg),
          None,
          ruleArg.rule.body.map(LiteralArgument) + LiteralArgument(ruleArg.rule.head),
          Move.OB2,
          Some("case a)")
      )
    )

    // case b)
    val caseB = (defencesContraries.intersect(framework.assumptions) -- (framework.culprits ++ framework.defences)).map(LiteralArgument) // definition
      .diff(dState.bLitArgs)  // prevent from repeating
      .toSeq.sortBy(_.lit.id)
      .map( litArg =>       PotentialMove(None, Some(litArg), Set(litArg), Move.OB2, Some("case b)")) )

    caseA ++ caseB
  }
}
