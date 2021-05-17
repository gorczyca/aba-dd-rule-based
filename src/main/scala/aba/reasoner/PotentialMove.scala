package aba.reasoner

import aba.move.Move.MoveType

case class PotentialMove(i: Int, moveType: MoveType) extends Ordered[PotentialMove] {
  override def compare(that: PotentialMove): Int = this.moveType compare that.moveType
}
