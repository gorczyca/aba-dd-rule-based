package aba.move

import scala.language.implicitConversions

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialMove}

import scala.math.Ordering.Implicits.seqOrdering

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

  // TODO: change MoveType to Value?
  def getPossibleMoves(implicit framework: Framework, dState: DisputeState): Map[MoveType, Seq[PotentialMove]] = {
    Move.values.map(x => (x, Move(x).isPossible)).filter(_._2.nonEmpty).toMap
  }

  implicit def fromString(moveString: String): MoveType = values.find(_.toString.equalsIgnoreCase(moveString)) match {
    case Some(value) => value
    case None => throw new Exception("TODO:")
  }

  implicit class PlayersMove(moveType: MoveType) {
    def isOpponentsMove: Boolean = !isProponentMove
    def isProponentMove: Boolean = moveType.toString.startsWith("P")
  }

  def possibleMovesToString(possibleMoves: Map[MoveType, Seq[PotentialMove]]): String = {
    possibleMoves.toSeq.sortBy(_._1).map {
      case (mType, moves) => s"$mType:\n" + moves.zipWithIndex.map {
        case (pMove, index) => s"\t$index: $pMove"
      }.mkString("\n")
    }.mkString("\n")
  }
}

// Abstract class for all moves
abstract class Move {
  //def isPossible(dState: DisputeState)(implicit framework: Framework): Set[PotentialMove]
  def isPossible(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove]
}
