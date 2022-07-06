package interface.reasoners


import aba.reasoner.automatic2.DisputeStateAuto2
import interface.ProgramState

object ApproximateReasonerInterface {

  def findSuccessfulApproximateDerivation(implicit programState: ProgramState): Unit = {

    val (tc, ad) = programState.approximateReasoner.automaticReasoner2.initialTCAndDA
    val initialDStateAuto = new DisputeStateAuto2(programState.currentDState, Set.empty, Set.empty, Nil, tc, ad)
    val initialDSs = List(initialDStateAuto)

    programState.approximateReasoner.getNewIncompleteSuccessfulDSAndStackRec(initialDSs, Nil, None)(programState.framework, onlyOne = false, None) match {
      case (_, list@successfulHead :: _, true, duration) =>
        println("Timeout reached")
      case (Nil, list@successfulHead :: _, _, duration) =>
        //println(s"Found all ${list.size} in $duration.")
        println(successfulHead.performedMovesToString.mkString("\n"))
        // TODO: uncomment that
        val defences = list.map(_.dState.defences).toSet
        //println(s" Distinct defences: ${defences.size}")
        //println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
      case (Nil, Nil, _, duration) =>
        println(s"Nothing found in $duration.")
      case (_, succHead :: _, _, duration) =>
        println("Something found") // TODO: extend, do "find next"
    }
  }

}
