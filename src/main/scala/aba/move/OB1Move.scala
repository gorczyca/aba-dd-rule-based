package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OB1Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {

    val union = framework.contrariesOf(framework.defences) union set
    val unblockedPiecesSupportingDefContrariesLit = framework.
      unblockedPiecesSupportingStatements(union).collect { case litArg: LiteralArgument => litArg.lit }
                                                                                                  // rule argument can only have children that are litArgs, hence no need to map
    framework.remainingNonBlockedBRules.filter(rule => unblockedPiecesSupportingDefContrariesLit.contains(rule.head))
      .map(RuleArgument)
      // previously we had:
      //.filter(_.children.intersect(framework.criticalPieces).nonEmpty) // by definition
      .diff(dState.bRuleArgs) // prevent from repeating
      .toSeq.sortBy(_.rule.head.id)
      .map(ruleArg => PotentialMove(Some(ruleArg), None, ruleArg.rule.body.map(LiteralArgument), Move.OB1, None))
  }
}
