package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object OF1Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    val completePiecesBLit = framework.completePiecesB.collect { case litArg: LiteralArgument => litArg.lit }
    framework.remainingNonBlockedBRules
      .filter(_.body.subsetOf(completePiecesBLit)).map(
      rule => PotentialMove(Some(RuleArgument(rule)), Set(LiteralArgument(rule.head)), Move.OF1, None)
    )
  }
}
