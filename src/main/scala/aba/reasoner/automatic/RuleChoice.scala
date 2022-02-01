package aba.reasoner.automatic

object RuleChoice extends Enumeration {
  type RuleChoiceType = Value

  val
  NewlyIntroducedStatementsMin,
  NewlyIntroducedStatementsMax,
  NewlyIntroducedAssumptionsMin,
  NewlyIntroducedAssumptionsMax
  = Value
}
