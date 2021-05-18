package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object PF2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    framework.assumptions.diff(dState.pLitArgs.map(_.lit) ++ framework.blockedAssumptionsP).map(
      lit => PotentialMove(None, Set(LiteralArgument(lit)), Move.PF2, None)
    )
  }
}
