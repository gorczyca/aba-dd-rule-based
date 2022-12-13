package aba.reasoner.automatic

import aba.move.DisputeAdvancement.{DABF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.automatic2.RuleChoice2.{RuleChoiceType2, NewlyIntroducedStatementsMin}
import aba.reasoner.automatic2.movesPreferenceBased.RuleHeadChoice.{LeastRules, RuleHeadChoiceType}
import aba.reasoner.automatic2.movesPreferenceBased.MovesPreferenceBasedAutomaticReasoner2
import aba.reasoner.automatic2.statementBased.TurnChoice2.{Proponent, TurnChoiceType2}


object AutomaticReasonerMock {

  val dfs: Boolean = true

  val tCriteriaType: TerminationCriteriaType = TA

  val dAdvancementType: DisputeAdvancementType = DABF

  val startWithAdmissible: Boolean = true

  val turnChoice: TurnChoiceType2 = Proponent

  val pRuleChoice: RuleChoiceType2 = NewlyIntroducedStatementsMin

  val oRuleChoice: RuleChoiceType2 = NewlyIntroducedStatementsMin

  val preferenceOrdering: Seq[MoveType] = Seq(PF1, PB2, PF2, PB1, OB2, OF2, OB1, OF1)

  val pRuleHeadChoice: RuleHeadChoiceType = LeastRules

  val oRuleHeadChoice: RuleHeadChoiceType = LeastRules

  val initialAutomaticReasoner: MovesPreferenceBasedAutomaticReasoner2 = MovesPreferenceBasedAutomaticReasoner2.default(tCriteriaType, dAdvancementType)

  val reasoner: MovesPreferenceBasedAutomaticReasoner2 =
    MovesPreferenceBasedAutomaticReasoner2(
      dfs,
      tCriteriaType,
      dAdvancementType,
      startWithAdmissible,
      preferenceOrdering,
      pRuleHeadChoice,
      oRuleHeadChoice,
      pRuleChoice,
      oRuleChoice
  )


}
