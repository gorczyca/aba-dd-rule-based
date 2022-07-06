package aba.reasoner.TOREMOVEautomatic

object RuleChoice extends Enumeration {
  type RuleChoiceType = Value

  val
  NewlyIntroducedStatementsMin,
  NewlyIntroducedStatementsMax,
  NewlyIntroducedAssumptionsMin,
  NewlyIntroducedAssumptionsMax
  = Value
}
