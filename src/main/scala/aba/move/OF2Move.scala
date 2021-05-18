package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object OF2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Set[PotentialMove] = {
    framework.assumptions.diff(framework.culprits ++ framework.defences).map(
      lit => PotentialMove(None, Set(LiteralArgument(lit)), Move.OF2, None)
    )
  }
}
