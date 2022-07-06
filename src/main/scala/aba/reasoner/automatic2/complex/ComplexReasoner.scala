package aba.reasoner.automatic2.complex

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialMove2}
import aba.reasoner.automatic2.{AutomaticReasoner2, DisputeStateAuto2}

abstract class ComplexReasoner(automaticReasoner: AutomaticReasoner2) {

  protected def findSuccessfullCompleteDisputeDerivations(constraints: Set[String], onlyOne: Boolean = false)(implicit framework: Framework): List[DisputeStateAuto2] = {
    // TODO: ensure TC / DC

    val blockedAssumptionsFramework = framework.copy(constraints = constraints)

    val (tc, ad) = automaticReasoner.initialTCAndDA

    val dState = DisputeState.initial(framework, Some(framework.assumptions -- constraints))
    implicit val initialDStateAuto: DisputeStateAuto2 = new DisputeStateAuto2(dState, Set.empty, Set.empty, Nil, tc, ad)

    // should not be a timeout
    val (_, successfulDS, _, _)  = automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(List(initialDStateAuto), Nil, None)(blockedAssumptionsFramework, onlyOne, None) // TODO: handle timeouts
    successfulDS
  }
}
