package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialMove}

// Companion object, holding all static things
object Move extends Enumeration {
  type MoveType = Value
  val
    PB1,
    PB2,
    PF1,
    PF2,
    OB1,
    OB2,
    OF1,
    OF2
  = Value

  def apply(moveType: MoveType): Move = {
    moveType match {
      case OB1 => OB1Move
      case OB2 => OB2Move
      case OF1 => OF1Move
      case OF2 => OF2Move
      case PB1 => PB1Move
      case PB2 => PB2Move
      case PF1 => PF1Move
      case PF2 => PF2Move
    }
  }

  def withNameOpt(moveString: String): Option[Value] = values.find(_.toString.equalsIgnoreCase(moveString))

  implicit class PlayersMove(moveType: MoveType) {
    def isOpponentsMove: Boolean = !isProponentMove
    def isProponentMove: Boolean = moveType.toString.startsWith("P")
  }

  def perform(dummy: Int): Int = {
    5
  }
}

// Abstract class for all moves
abstract class Move {
  //def isPossible(dState: DisputeState)(implicit framework: Framework): Set[PotentialMove]
  def isPossible(dState: DisputeState)(implicit framework: Framework): Set[PotentialMove]
}
