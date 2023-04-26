package aba.move

import scala.language.implicitConversions // needed for the fromString method

import aba.framework.Framework
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.reasoner.{DisputeState, PotentialMove2}

object DisputeAdvancement extends Enumeration {
  type DisputeAdvancementType = Value

  val
    DAB,
    DABF,
    DC,
    DS,
    DF
  = Value

  implicit def fromString(advancementTypeString: String): DisputeAdvancementType = values.find(_.toString.equalsIgnoreCase(advancementTypeString)) match {
    case Some(value) => value
    case None => throw new Exception(s"No advancement type: $advancementTypeString")
  }


  def apply(advancementType: DisputeAdvancementType): DisputeAdvancement = {
    val moves = advancementType match {
      case DF => Seq(
        PB1,
        PB2,
        PF1,
        PF2,
        OB1,
        OB2,
        OF1,
        OF2,
      )
      case DAB => Seq(
        PB1,
        PB2,
        PF2,
        OB1,
        OB2,
        OF2,
      )
      case DABF => Seq(
        PB1,
        PB2,
        PF1,
        PF2,
        OB1,
        OB2,
        OF2,
      )
// TODO: when allowing for OF1 moves, and disallowing PF1
//      case DABF => Seq(
//        PB1,
//        PB2,
//        //PF1,
//        PF2,
//        OF1,
//        OB1,
//        OB2,
//        OF2,
//      )
      case DC => Seq(
        PB1,
        PB2,
        PF1,
        PF2,
        OB1,
        OB2,
        OF2,
      )
      case DS => Seq(
        PB1,
        PB2,
        PF1,
        PF2,
        OB1,
        OB2,
        OF2,
      )
    }
    DisputeAdvancement(advancementType, moves)
  }
}


case class DisputeAdvancement(advancementType: DisputeAdvancementType, moves: Seq[MoveType]) {

  def getPossibleMoves(implicit framework: Framework, dState: DisputeState): Map[MoveType, Seq[PotentialMove2]] = {
    moves.map(moveType => (moveType, Move(moveType).isPossible(advancementType))).filter(_._2.nonEmpty).toMap
  }
}
