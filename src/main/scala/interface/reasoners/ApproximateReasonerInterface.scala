package interface.reasoners


import aba.reasoner.{DisputeState, PotentialMove2}
import aba.reasoner.automatic2.DisputeStateAuto2
import interface.ProgramState

object ApproximateReasonerInterface {

  def findSuccessfulApproximateDerivation(implicit programState: ProgramState): (List[PotentialMove2], DisputeState) = {

    val (tc, ad) = programState.approximateReasoner.automaticReasoner2.initialTCAndDA
    val initialDStateAuto = new DisputeStateAuto2(programState.currentDState, Set.empty, Set.empty, Nil, tc, ad)
    val initialDSs = List(initialDStateAuto)

    programState.approximateReasoner.getNewIncompleteSuccessfulDSAndStackRec(initialDSs, Nil, None)(programState.framework, onlyOne = false, None) match {
      case (_, list@successfulHead :: _, true, duration) =>
        println("Timeout reached")
        (Nil, programState.currentDState)
      case (Nil, list@successfulHead :: _, _, duration) =>
        //println(s"Found all ${list.size} in $duration.")
        println(successfulHead.performedMovesToString().mkString("\n"))
        // TODO: uncomment that
        val defences = list.map(_.dState.defences).toSet
        //println(s" Distinct defences: ${defences.size}")
        //println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
        (successfulHead.performedMoves, successfulHead.dState)

      case (Nil, Nil, _, duration) =>
        println(s"Nothing found in $duration.")
        (Nil, programState.currentDState)
      case (_, succHead :: _, _, duration) =>
        println("Something found") // TODO: extend, do "find next"
        (Nil, programState.currentDState) // TODO:
    }
  }

}
