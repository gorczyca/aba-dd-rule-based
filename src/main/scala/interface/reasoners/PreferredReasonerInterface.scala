package interface.reasoners


import interface.ProgramState

object PreferredReasonerInterface {

  def findSuccessfulDerivationsPreferred(implicit programState: ProgramState): Unit = {

    // TODO: clean
    val framework = programState.framework
    val frameworkWithoutGoals = framework.copy(goals = Set.empty)


    // TODO: return something

    println("Finding a successful derivation. This can take a moment...")

    programState.preferredReasoner.findSuccessfulDDs(List(framework.assumptions), Nil, Set.empty, Set.empty, framework.goals, onlyOne = true, timeoutOpt = None)(frameworkWithoutGoals) match {
      case (Nil, _, _) =>
        println("No successful derivations found")
      case (list, timeout, duration) =>
        if (timeout) println("Timed out") else println(s"Finished in $duration")
        println(s"Found: ${list.size}")
        val defences = list.map(_.dState.defences).toSet
        println(s" Distinct defences: ${defences.size}")
        println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))

        // TODO: add option here to find next
    }
  }

}
