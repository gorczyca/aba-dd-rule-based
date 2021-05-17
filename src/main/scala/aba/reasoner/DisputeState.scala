package aba.reasoner

import aba.move.Move.MoveType

case class DisputeState(id: Int,
                        move: Option[MoveType],
                        p: Set[Argument],
                        b: Set[Argument]
                       ) {

  def this(goals: Set[Argument]) = {
    this(0, None, goals, Set.empty)
  }

}
