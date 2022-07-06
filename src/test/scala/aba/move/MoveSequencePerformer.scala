package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialMove2}

import scala.annotation.tailrec

object MoveSequencePerformer {

  def apply(moves: Seq[PotentialMove2], currentState: DisputeState, framework: Framework): DisputeState = moves match {
    case Nil => currentState
    case move :: rest =>
      val newCurrentState = move.perform(currentState, framework)
      apply(rest, newCurrentState, framework)
  }

}
