package aba.reasoner.structured

import aba.reasoner.argumentBased2.ArgumentTree

class HighlightedData(val proponent: Boolean)

case class StatementsToChooseFrom(statements: Set[String], override val proponent: Boolean) extends HighlightedData(proponent)
case class ArgumentsToChooseFrom(arguments: Set[ArgumentTree], override val proponent: Boolean) extends HighlightedData(proponent)
case class StatementsWithinArgumentToChooseFrom(argumentTree: Set[ArgumentTree], statements: Set[String], override val proponent: Boolean) extends HighlightedData(proponent)
//case class SelectedStatementWithinArgument(argumentTree: Set[ArgumentTree], statement: Set[String], override val proponent: Boolean) extends HighlightedData(proponent)


