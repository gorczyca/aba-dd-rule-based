package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object OF2Move extends Move {
  override def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {
    framework.assumptions.diff(framework.culprits ++ framework.defences).map(LiteralArgument) // definition
      .diff(dState.bLitArgs) // prevent from repeating
      .toSeq.sortBy(_.lit.id) // sorting
      .map( litArg => PotentialMove(None, Some(litArg), Set(litArg), Move.OF2, None) )
  }
}
