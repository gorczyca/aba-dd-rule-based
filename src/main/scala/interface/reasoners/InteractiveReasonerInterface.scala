package interface.reasoners

import aba.move.Move.MoveType
import aba.reasoner.argumentBased2.DisputeStateAB2
import aba.reasoner.automatic2.DisputeStateAuto2
import interface.ProgramState
import aba.reasoner.{PotentialAssumptionMove, PotentialMove2, PotentialRuleMove}

object InteractiveReasonerInterface {

  def getNewStateInteractively(proponentsMove: Boolean)(implicit programState: ProgramState): ProgramState = {

    // TODO: clean
    val framework = programState.framework
    val potentialMoves = programState.possibleMoves

    val newDSAuto = if(proponentsMove) {

      val initialDSAuto = new DisputeStateAuto2(programState.currentDState, Set.empty, Set.empty, Nil, programState.tCriteria, programState.dAdvancement, Some(Set.empty))
      programState.interactiveReasoner.getNewProponentArgs(initialDSAuto, framework, potentialMoves, Some(1))

    } else {

      val initialDSAuto = new DisputeStateAuto2(programState.currentDState, Set.empty, Set.empty, Nil, programState.tCriteria, programState.dAdvancement, None)
      programState.interactiveReasoner.getNewOpponentArgs(initialDSAuto, framework, potentialMoves)
    }

    if (newDSAuto.performedMoves.isEmpty) {
      println("This had no effect. Try with the other player." )
      programState.copy(redraw = false)
    } else {


      // TODO
      //val over = programState.interactiveReasoner.checkIfOver(framework, newDSAuto.dState)
      //val over = programState.interactiveReasoner.checkIfOver(framework, newDSAuto.dState)
      //over match {
      //  case Some(true) => println("Proponent won.")
      //  case Some(false) => println("Opponent won")
      //  case _ =>
      //}

      val newPerformedMoves = programState.performedMoves ++ newDSAuto.performedMoves
      val newPerformedMovesChunks = programState.performedMovesChunks :+ newDSAuto.performedMoves



      // convert the performed moves to argument
      // print the added argument

      val lastArgString = newDSAuto.performedMoves.head match {
        case x: PotentialRuleMove =>
          DisputeStateAB2.create_arguments(Set(x.rule.head), newDSAuto.performedMoves.map {
            case rMove: PotentialRuleMove => rMove.rule
          }.toSet)(framework).map(_.toString).mkString("\n")
        case a: PotentialAssumptionMove =>
          a.assumption
      }

      println(lastArgString)

//      val argument = DisputeStateAB2.create_arguments(Set(stmt), relevantRules)


      programState.copy(
        currentDState = newDSAuto.dState,
        performedMoves = newPerformedMoves,
        performedMovesChunks = newPerformedMovesChunks,
        redraw = false
        //interactiveOver = over
      )
    }
  }
}
