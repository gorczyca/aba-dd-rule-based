package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object PF2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    framework.assumptions.diff(dState.pLitArgs.map(_.lit) ++ framework.blockedAssumptionsP).map(LiteralArgument) // definition
      .diff(dState.pLitArgs)  // prevent from repeating
      .toSeq.sortBy(_.lit.id) // sorting
      .map( litArg => PotentialMove(None, Some(litArg), Set(litArg), Move.PF2, None) )
  }
}
