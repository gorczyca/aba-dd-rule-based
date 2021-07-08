package aba.move

import aba.framework.{Framework, Literal}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.reasoner.{DisputeState, PotentialMove}

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

  //def advancementTypesPartialOrder = Seq(DAB, DABF, Array(DC, DS), DF)

  def apply(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): DisputeAdvancement = {
    val moves = advancementType match {
      case DF => Seq(
        (PB1, framework.contrariesOf(framework.assumptions)),
        (PB2, framework.contrariesOf(framework.assumptions)),
        (PF1, framework.assumptions),
        (PF2, framework.assumptions),
        (OB1, framework.contrariesOf(framework.assumptions)),
        (OB2, framework.contrariesOf(framework.assumptions)),
        (OF1, framework.assumptions),
        (OF2, framework.assumptions),
      )
      case DAB => Seq(
        (PB1, Set.empty[Literal]),
        (PB2, Set.empty[Literal]),
        (PF2, Set.empty[Literal]),
        (OB1, Set.empty[Literal]),
        (OB2, Set.empty[Literal]),
        (OF2, Set.empty[Literal]),
      )
      case DABF => Seq(
        (PB1, Set.empty[Literal]),
        (PB2, Set.empty[Literal]),
        (PF1, Set.empty[Literal]),
        (PF2, Set.empty[Literal]),
        (OB1, Set.empty[Literal]),
        (OB2, Set.empty[Literal]),
        (OF2, Set.empty[Literal]),
      )
      case DC => Seq(
        (PB1, Set.empty[Literal]),
        (PB2, Set.empty[Literal]),
        (PF1, framework.j),
        (PF2, framework.j),
        (OB1, framework.contrariesOf(framework.j)),
        (OB2, framework.contrariesOf(framework.j)),
        (OF2, Set.empty[Literal]),
      )
      case DS => Seq(
        (PB1, framework.contrariesOf(framework.assumptions)),
        (PB2, framework.contrariesOf(framework.assumptions)),
        (PF1, framework.assumptions),
        (PF2, framework.assumptions),
        (OB1, Set.empty[Literal]),
        (OB2, Set.empty[Literal]),
        (OF2, Set.empty[Literal]),
      )
    }
    DisputeAdvancement(advancementType, moves)
  }
}


case class DisputeAdvancement(advancementType: DisputeAdvancementType,
                              moves: Seq[(MoveType, Set[Literal])]) {

  def getPossibleMoves(implicit framework: Framework, dState: DisputeState): Map[MoveType, Seq[PotentialMove]] = {
    moves.map { case (moveType, set) => (moveType, Move(moveType).isPossible(set)) }.filter(_._2.nonEmpty).toMap
  }
}
