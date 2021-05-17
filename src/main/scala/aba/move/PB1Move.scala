package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialMove}

object PB1Move extends Move {
  override def isPossible(dState: DisputeState)(implicit framework: Framework): Set[PotentialMove] = ???

}
