package interface.reasoners

import aba.reasoner.automatic2.DisputeStateAuto2
import interface.ProgramState

object InteractiveReasonerInterface {

  def getNewStateInteractively(proponentsMove: Boolean)(implicit programState: ProgramState): ProgramState = {

    // TODO: clean
    val framework = programState.framework

    val newDSAuto = if(proponentsMove) {
      val initialDSAuto = new DisputeStateAuto2(programState.currentDState, Set.empty, Set.empty, Nil, programState.tCriteria, programState.dAdvancement, Some(Set.empty))
      programState.interactiveReasoner.getNewProponentArgs(initialDSAuto, framework, Some(1))

    } else {
      val initialDSAuto = new DisputeStateAuto2(programState.currentDState, Set.empty, Set.empty, Nil, programState.tCriteria, programState.dAdvancement, None)
      programState.interactiveReasoner.getNewOpponentArgs(initialDSAuto, framework)
    }

    if (newDSAuto.performedMoves.isEmpty) {
      println("This had no effect. Try with the other player." )
      programState
    } else {

      val over = programState.interactiveReasoner.checkIfOver(framework, newDSAuto.dState)
      over match {
        case Some(true) => println("Proponent won.")
        case Some(false) => println("Opponent won")
        case _ =>
      }

      val newPerformedMoves = programState.performedMoves ++ newDSAuto.performedMoves
      val newPerformedMovesChunks = programState.performedMovesChunks :+ newDSAuto.performedMoves

      programState.copy(
        currentDState = newDSAuto.dState,
        performedMoves = newPerformedMoves,
        performedMovesChunks = newPerformedMovesChunks,
        interactiveOver = over
      )
    }
  }
}
