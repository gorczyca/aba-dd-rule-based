package interface.reasoners


import aba.reasoner.automatic2.DisputeStateAuto2
import aba.reasoner.{DisputeState, PotentialMove2}
import interface.InputProcessorInterface.getUserInput
import interface.dotConverters.ABRepresentationInterface.generateABRepresentation
import interface.ProgramState

import scala.annotation.tailrec

object AutomaticReasonerInterface {

  def findSuccessfulDerivations2(onlyOne: Boolean, performNMoves: Option[Int] = None, generateABRep: Boolean = false, findAndReturn: Boolean = false, quiet: Boolean = false)(implicit programState: ProgramState): ProgramState = {

    // TODO: clean
    val framework = programState.framework
    val currentState = programState.currentDState

    (performNMoves, quiet) match {
      case (None, false) => println("Finding a successful derivation. This can take a moment...")
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
          // case: timeout reached

          // TODO: do i need that?
          println("Timeout reached")
//          println(s"(After) $duration")
//          println(s" Found: ${list.size}")
//          val defences = list.map(_.dState.defences).toSet
//          println(s" Distinct defences: ${defences.size}")
//          println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
          (Nil, currentState)

        case (Nil, Nil, _, _) =>
          // case: unsatisfiable
          if (quiet) println("NO") else println("No successful derivations found")
          (Nil, currentState)

        case (Nil, list@successfulHead::_, _, duration) =>
          if (quiet) println("YES")
          else {
            println(s"Finished in $duration")
            println(successfulHead.performedMovesToString(programState.performedMoves.size).mkString("\n"))
          }

          //programState.performedMoves.size

          // TODO: uncomment that
          //println(s" All successful found: ${list.size}")
          //val defences = list.map(_.dState.defences).toSet
          //println(s" Distinct defences: ${defences.size}")
          //println(defences.toList.sortBy(_.size).map(group => s"""[ ${group.mkString(",")} ]""").mkString("\n"))
          (successfulHead.performedMoves, successfulHead.dState)

        case (restDS, successfulHead :: _, _, duration) =>
          if (quiet) println("YES")
          else {
            println(s"Finished in $duration")
            println(successfulHead.performedMovesToString().mkString("\n"))
          }
          // TODO: this is the culprit - this should be only explicitly called for
          if (generateABRep) generateABRepresentation(over = Some(true))(programState.copy(currentDState = successfulHead.dState))
          if (!findAndReturn) {
            println("Press ENTER to finish, ; to find another one")
            getUserInput match {
              case ";" => findSuccessfulDerivationsRec2(restDS)
              case _ => (successfulHead.performedMoves, successfulHead.dState)
            }
          } else (successfulHead.performedMoves, successfulHead.dState)
        case (statesAfterNMoves, Nil, _, _) => // TODO: at worst add if to the case

          // TODO: when doing this, we do not want ignoring moves, because it does not say anything
          //  possible fixes:
          //  1) filter out ingoring moves
          //  2) allow them, but indicate this

          performNMoves match {
            case Some(n) =>
              println(s"${n} moves performed.")
              val chosenState = statesAfterNMoves.head
              println(chosenState.performedMovesToString().mkString("\n"))

              val numberLength = chosenState.performedMoves.length.toString.length
              val movesNumber = chosenState.performedMoves.length
              if ((chosenState.ignoredProponentAssumptions ++ chosenState.ignoredCulpritCandidates).nonEmpty) {
                chosenState.ignoredProponentAssumptions.zipWithIndex.foreach {
                  case (ass, index) => println(s"%0${numberLength}d".format(index + 1 + movesNumber) + s": Ignored prop. assumption\t[${ass}]")
                }
                val ignoredPropAssumptionsSize = chosenState.ignoredProponentAssumptions.size
                chosenState.ignoredCulpritCandidates.zipWithIndex.foreach {
                  case (ass, index) => println(s"%0${numberLength}d".format(index + 1 + movesNumber + ignoredPropAssumptionsSize) + s": Ignored culprit candidate\t[${ass}]")
                }
              }
              (chosenState.performedMoves, chosenState.dState)
          }
      }
    }

    val (newPerformedMoves, newDState) = findSuccessfulDerivationsRec2(initialDSs)

    programState.copy(
      performedMoves = programState.performedMoves ++ newPerformedMoves,
      currentDState = newDState,
      redraw = false
    )
  }
}
