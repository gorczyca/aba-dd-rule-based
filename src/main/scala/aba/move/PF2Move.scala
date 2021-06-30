package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object PF2Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {

    val union = (framework.contrariesOf(framework.culpritsCandidates) intersect framework.assumptions) union set
    val unionContraries = framework.contrariesOf(union)
    val setDiff =  dState.pLitArgs.map(_.lit) union unionContraries union framework.culprits union framework.contrariesOf(framework.defences) union framework.constraints

    (union -- setDiff).map(LiteralArgument) // definition
      .diff(dState.pLitArgs)  // prevent from repeating
      .toSeq.sortBy(_.lit.id) // sorting
      .map( litArg => PotentialMove(None, Some(litArg), Set(litArg), Move.PF2, None) )

  }
}
