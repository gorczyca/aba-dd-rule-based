package aba.reasoner.TOREMOVEautomatic

object StatementChoice extends Enumeration {
  type StatementChoiceType = Value

  val
  Eager,
  Patient,
  Newest,
  Oldest
  = Value
}
