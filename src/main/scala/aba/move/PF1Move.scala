package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove, RuleArgument}

object PF1Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    val completePiecesPLit = framework.completePiecesP.collect { case litArg: LiteralArgument => litArg.lit }
    framework.remainingNonBlockedPRules
      .filter( rule => !dState.pLitArgs.map(_.lit).contains(rule.head) && rule.body.subsetOf(completePiecesPLit)).map(
        rule => PotentialMove(Some(RuleArgument(rule)), Set(LiteralArgument(rule.head)), Move.PF1, None)
    )
  }
}
