package aba.reasoner

import aba.move.Move.MoveType

case class DisputeState(id: Int,
                        move: MoveType,
                        p: Set[Argument],
                        b: Set[Argument]
                       )