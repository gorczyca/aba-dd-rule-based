package aba.move

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
      case OB1 => new OB1Move()
      case OB2 => new OB2Move()
      case OF1 => new OF1Move()
      case OF2 => new OF2Move()
      case PB1 => new PB1Move()
      case PB2 => new PB2Move()
      case PF1 => new PF1Move()
      case PF2 => new PF2Move()
    }
  }


}

abstract class Move {
  def isPossible(dummy: Int): Boolean
  def perform(dummy: Int): Int
}
