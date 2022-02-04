package aba.move

import aba.framework.{Framework, Literal}
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}

object PF2Move extends Move {
  override def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove] = {

    val varUnion = ((framework.contrariesOf(framework.culpritsCandidates) intersect framework.assumptions) union set)
      .filterNot(lit => framework.contrariesOf(lit).contains(lit)) // filter self-contradicting
    val setDiff =  dState.pLitArgs.map(_.lit) union framework.culprits union framework.contrariesOf(framework.defences) union framework.constraints

    (varUnion -- setDiff).map(LiteralArgument) // definition
      .diff(dState.pLitArgs)  // prevent from repeating
      .toSeq.sortBy(_.lit.id) // sorting
      .map( litArg => PotentialMove(None, Some(litArg), Set(litArg), Move.PF2, None,
        if (framework.culpritsCandidates.exists(c => framework.contrariesOf(c).contains(litArg.lit))) Some(framework.culpritsCandidates.filter(c => framework.contrariesOf(c).contains(litArg.lit)) )
        else None ))

  }
}
