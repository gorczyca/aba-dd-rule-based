package interface.reasoners


import aba.reasoner.automatic2.DisputeStateAuto2
import aba.reasoner.{DisputeState, PotentialMove2}
import interface.ProgramState

import scala.annotation.tailrec

object AutomaticReasonerInterface {

  def findSuccessfulDerivations2(onlyOne: Boolean, performNMoves: Option[Int] = None)(implicit programState: ProgramState): ProgramState = {

    // TODO: clean
    val framework = programState.framework
    val currentState = programState.currentDState

    performNMoves match {
      case None => println("Finding a successful derivation. This can take a moment...")
      case _ =>
    }

    // TODO: get initial state somehow from reasoner?
    val (tc, ad) = programState.automaticReasoner.initialTCAndDA
    val initialDStateAuto = new DisputeStateAuto2(currentState, Set.empty, Set.empty, Nil, tc, ad)
    val initialDSs = List(initialDStateAuto)

    @tailrec
    def findSuccessfulDerivationsRec2(stack: List[DisputeStateAuto2]): (List[PotentialMove2], DisputeState) = {

      programState.automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(stack, Nil, None, (isIgnored, _) => (x: PotentialMove2) => !isIgnored(x), performNMoves)(framework, onlyOne = onlyOne, None) match {
        // TODO: remove the three last
        case (_,  list@_::_, true, duration) =>
          println("Timeout reached")
          println(s"(After) $duration")
          println(s" Found: ${list.size}")
          val defences = list.map(_.dState.defences).toSet
          println(s" Distinct defences: ${defences.size}")
          println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
          (Nil, currentState)

        case (Nil, Nil, _, _) =>
          println("No successful derivations found")
          (Nil, currentState)

        case (Nil, list@successfulHead::_, _, duration) =>
          println(s"Finished in $duration")
          println(successfulHead.performedMovesToString.mkString("\n"))
          // TODO: uncomment that
          //println(s" All successful found: ${list.size}")
          val defences = list.map(_.dState.defences).toSet
          //println(s" Distinct defences: ${defences.size}")
          //println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
          (successfulHead.performedMoves, successfulHead.dState)

        case (restDS, successfulHead :: _, _, duration) =>
          println(s"Successful derivation found in $duration.")
          println(successfulHead.performedMovesToString.mkString("\n"))
          println("Press ENTER to finish, ; to find another one")
          Console.in.readLine match {
            case ";" => findSuccessfulDerivationsRec2(restDS)
            case _ => (successfulHead.performedMoves, successfulHead.dState)
          }
        case (statesAfterNMoves, Nil, _, _) => // TODO: at worst add if to the case
          performNMoves match {
            case Some(n) =>
              println(s"${n} moves performed.")
              val chosenState = statesAfterNMoves.head
              println(chosenState.performedMovesToString.mkString("\n"))
              (chosenState.performedMoves, chosenState.dState)
          }
      }
    }

    val (newPerformedMoves, newDState) = findSuccessfulDerivationsRec2(initialDSs)

    programState.copy(
      performedMoves = programState.performedMoves ++ newPerformedMoves,
      currentDState = newDState
    )
  }
}
