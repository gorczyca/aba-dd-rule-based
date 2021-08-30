package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object OF2Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {

    val playedAssumptions = dState.bLitArgs.map(_.lit) intersect framework.assumptions
    val contrariesDefencesUnion = framework.contrariesOf(framework.defences) union set

    // restrict only to assumptions
    (contrariesDefencesUnion intersect framework.assumptions).diff(playedAssumptions ++ framework.culprits)
      .map(LiteralArgument)
      .diff(dState.bLitArgs) // prevent from repeating
      .toSeq.sortBy(_.lit.id) // sorting
      .map( litArg => PotentialMove(None, Some(litArg), Set(litArg), Move.OF2, None) )
  }
}
