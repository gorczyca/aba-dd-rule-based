package aba.reasoner.automatic

object TurnChoice extends Enumeration {
  type TurnChoiceType = Value

  val
  Proponent,
  Opponent,
  SmallestStatementSet,
  LargestStatementSet
  = Value
}
