package interface.reasoners

import interface.ProgramState

object GroundedReasonerInterface {

  def findSuccessfulDerivationsGrounded(onlyOne: Boolean = false)(implicit programState: ProgramState): Unit = {

    // TODO: clean
    val framework = programState.framework
    val frameworkWithoutGoals = framework.copy(goals = Set.empty)

    // TODO: return something from this

    println("Finding a successful derivation. This can take a moment...")

    programState.groundedReasoner.findGroundedDisputeDerivations(0, List(Set.empty), timeoutOpt = None)(frameworkWithoutGoals, onlyOne) match {
      case (Nil, _, _) =>
        println("No successful derivations found")
      case (list, timeout, duration) =>
        val groundedSupportingGoals = list.filter(d => framework.goals.subsetOf(d.dState.pPlayedCompleteStatements))
        if (timeout) println("Timed out") else println(s"Finished in $duration")
        println(s"Found: ${groundedSupportingGoals.size}")
        val defences = groundedSupportingGoals.map(_.dState.defences).toSet
        println(s" Distinct defences: ${defences.size}")
        println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
    }
  }
}
