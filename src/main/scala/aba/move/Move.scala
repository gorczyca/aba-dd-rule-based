package aba.move

import scala.language.implicitConversions
import aba.framework.{Framework, Literal}
import aba.move.DisputeAdvancement.{DAB, DABF, DC, DF, DS, DisputeAdvancementType}
import aba.reasoner.{DisputeState, PotentialMove}

import scala.math.Ordering.Implicits.seqOrdering

// Companion object, holding all static things
object Move extends Enumeration {
  // TODO: consider changing Enumeration to sealed trait
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

  implicit def fromString(moveString: String): MoveType = values.find(_.toString.equalsIgnoreCase(moveString)) match {
    case Some(value) => value
    case None => throw new Exception(s"No move type: $moveString")
  }

  implicit class PlayersMove(moveType: MoveType) {
    def isOpponentsMove: Boolean = !isProponentMove
    def isProponentMove: Boolean = moveType.toString.startsWith("P")

    def isForwardMove: Boolean = !isBackwardMove
    def isBackwardMove: Boolean = moveType.toString()(1) == 'B'

    def isRuleMove: Boolean = Set(OB1, OB2, OF1, PB1, PB2, PF1).contains(moveType)
    def isAssumptionMove: Boolean = !isRuleMove
  }

  def possibleMovesToString(possibleMoves: Map[MoveType, Seq[PotentialMove]]): String = {
    possibleMoves.toSeq.sortBy(_._1).map {
      case (mType, moves) => s"$mType:\n" + moves.zipWithIndex.map {
        case (pMove, index) => s"\t$index: $pMove"
      }.mkString("\n")
    }.mkString("\n")
  }

  def possibleMovesAccordingToAllAdvancementToString(implicit dState: DisputeState, framework: Framework): String = {
    val advancementTypesMoves = DisputeAdvancement.values
      .toSeq.sortWith(_ < _)
      .map(aType => (aType, DisputeAdvancement(aType).getPossibleMoves))

    val moveTypes = advancementTypesMoves.flatMap(_._2.keys).distinct.sortWith(_ < _)

    val x = moveTypes.map(mType => {
      val innerMap = advancementTypesMoves.filter(_._2.keys.toSeq.contains(mType))
        .map { case (advType: DisputeAdvancementType, map: Map[MoveType, Seq[PotentialMove]]) =>
          (advType, map(mType))
        }.sortWith(_._1 < _._1).toMap
      (mType, innerMap)
    })

    x.map { case (mType: MoveType, map: Map[DisputeAdvancementType, Seq[PotentialMove]] ) =>
      s"$mType:\n" + map.map {
        { case (dType, potentialMoves) => s"\t$dType:\n" + potentialMoves.map(pMove => s"\t\t$pMove").mkString("\n") }   // TODO: zip with index?
      }.mkString("\n")
    }.mkString("\n")
  }

}

// Abstract class for all moves
abstract class Move {
  //def isPossible(dState: DisputeState)(implicit framework: Framework): Set[PotentialMove]
  def isPossible(set: Set[Literal])(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove]
}
