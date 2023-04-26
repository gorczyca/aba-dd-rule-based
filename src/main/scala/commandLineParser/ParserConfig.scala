package commandLineParser

import aba.move.DisputeAdvancement.{DABF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.automatic2.RuleChoice2.{LookAhead1Step, NewlyIntroducedStatementsMin, RuleChoiceType2}
import aba.reasoner.automatic2.movesPreferenceBased.RuleHeadChoice.{LeastRules, MostRules, RuleHeadChoiceType}
import aba.reasoner.automatic2.statementBased.StatementChoice2.{Patient, StatementChoiceType2}
import aba.reasoner.automatic2.statementBased.TurnChoice2.{Proponent, TurnChoiceType2}

case class ParserConfig(
                         inputFilePath: String = "",
                         inputFormat: String = "aba",
                         goal: Option[String] = None,

                         solve: Boolean = false, // if true will solve automatically and print the result
                         quiet: Boolean = false, // if true will only print YES / NO
                         // default automatic reasoner options options
                         dfs: Boolean = true,
                         tCriteriaType: TerminationCriteriaType = TA,
                         dAdvancementType: DisputeAdvancementType = DABF,
                         startWithAdmissible: Boolean = true,
                         turnChoice: TurnChoiceType2 = Proponent,
                         pStatementChoice: StatementChoiceType2 = Patient,
                         oStatementChoice: StatementChoiceType2 = Patient,
                         pRuleChoiceType: RuleChoiceType2 = NewlyIntroducedStatementsMin,
                         //pRuleChoiceType: RuleChoiceType2 = LookAhead1Step,
                         oRuleChoiceType: RuleChoiceType2 = NewlyIntroducedStatementsMin,
                         //oRuleChoiceType: RuleChoiceType2 = LookAhead1Step,
                         preferenceOrdering: Seq[MoveType] = Seq(PF1, PB2, PF2, PB1, OB2, OF2, OB1),
                         //preferenceOrdering: Seq[MoveType] = Seq(PF1, OF1, OB2, OF2, OB1, PB1, PB2, PF2),
                         pRuleHeadChoice: RuleHeadChoiceType = LeastRules,
                         //oRuleHeadChoice: RuleHeadChoiceType = MostRules,
                         oRuleHeadChoice: RuleHeadChoiceType = LeastRules,
                       )
