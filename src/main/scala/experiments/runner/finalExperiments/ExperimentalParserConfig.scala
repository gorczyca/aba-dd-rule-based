package experiments.runner.finalExperiments

import aba.fileParser.FileParser
import aba.framework.Framework
import aba.move.DisputeAdvancement.{DAB, DisputeAdvancementType}
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.approximate.ApproximateReasoner
import aba.reasoner.automatic2.complex.{GroundedReasoner, PreferredReasoner}
import aba.reasoner.automatic2.RuleChoice2.{LookAhead1Step, RuleChoiceType2}
import aba.reasoner.automatic2.movesPreferenceBased.MovesPreferenceBasedAutomaticReasoner2
import aba.reasoner.automatic2.movesPreferenceBased.RuleHeadChoice.{LeastRules, MostRules, RuleHeadChoiceType}
import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2
import aba.reasoner.automatic2.statementBased.StatementChoice2.{Patient, StatementChoiceType2}
import aba.reasoner.automatic2.statementBased.TurnChoice2.{Proponent, TurnChoiceType2}
import experiments.runner.actualFinalExperiments.ExperimentsMode.{ExperimentsModeType, Grounded, Normal}

import scala.util.{Failure, Success}

case class ExperimentalParserConfig(
                                     frameworkInputPath: String = "",
                                     goal: Option[String] = None,
                                     inputFormat: String = "apx",
                                     mode: ExperimentsModeType = Normal,
                                     onlyOne: Boolean = true,
                                     outputArgRep: Boolean = false,
                                     // outputRuleRep: Boolean = false,
                                     csvInputPath: String = "", // TODO: remove
                                     instancesDirectory: String = "", // TODO: remove
                                     outputDirectory: String = ".", // TODO: keep
                                     propP: Double = 1.0,
                                     oppP: Double = 1.0,
                                     sampleBefore: Boolean = true,
                                     // default automatic reasoner options options
                                     timeout: Long = 120, // TODO: remove
                                     // always relevant
                                     dfs: Boolean = true,
                                     tCriteriaType: TerminationCriteriaType = TA,
                                     dAdvancementType: DisputeAdvancementType = DAB,
                                     startWithAdmissible: Boolean = true,

                                     // only relevant if mode = AbaStrategy
                                     turnChoice: TurnChoiceType2 = Proponent,
                                     pStatementChoice: StatementChoiceType2 = Patient,
                                     oStatementChoice: StatementChoiceType2 = Patient,
                                     // relevant in all automatic search, both AbaStrategy and normal
                                     pRuleChoiceType: RuleChoiceType2 = LookAhead1Step,
                                     oRuleChoiceType: RuleChoiceType2 = LookAhead1Step,
                                     // relevant when mode = Normal
                                     preferenceOrdering: Seq[MoveType] = Seq(PF1, OF1, OB2, OF2, OB1, PB1, PB2, PF2),
                                     pRuleHeadChoice: RuleHeadChoiceType = LeastRules,
                                     oRuleHeadChoice: RuleHeadChoiceType = MostRules,
                                   ) {

  import aba.move.Move.PB1

  def getStatementBasedReasoner: StatementBasedAutomaticReasoner2 = {
    StatementBasedAutomaticReasoner2(
      dfs = dfs,
      tCriteriaType = tCriteriaType,
      dAdvancementType = dAdvancementType,
      startWithAdmissible = startWithAdmissible,
      turnChoice = turnChoice,
      pStatementChoice = pStatementChoice,
      oStatementChoice = oStatementChoice,
      pRuleChoiceType = pRuleChoiceType,
      oRuleChoiceType = oRuleChoiceType
    )
  }

  def getAutomaticReasoner: MovesPreferenceBasedAutomaticReasoner2 = {
    MovesPreferenceBasedAutomaticReasoner2(
      dfs = dfs,
      tCriteriaType = tCriteriaType,
      dAdvancementType = dAdvancementType,
      startWithAdmissible = startWithAdmissible,
      preferenceOrdering = preferenceOrdering,
      pRuleHeadChoice = pRuleHeadChoice,
      oRuleHeadChoice = oRuleHeadChoice,
      pRuleChoice = pRuleChoiceType,
      oRuleChoice = oRuleChoiceType
    )
  }


  def getApproximateReasoner: ApproximateReasoner = {
    val automaticReasoner = getAutomaticReasoner

    FileParser(inputFormat, frameworkInputPath) match {
      case Success(value) =>
        implicit val framework: Framework = goal match {
          case Some(goal) => value.copy(goals = Set(goal))
          case _ => value
      }

      ApproximateReasoner(automaticReasoner, propP, oppP, framework,  sampleBefore)

      case Failure(exception) => throw exception
    }
  }


  def getGroundedReasoner: GroundedReasoner = {
    val automaticReasoner = getAutomaticReasoner
    new GroundedReasoner(automaticReasoner)
  }

  def getPreferredReasoner: PreferredReasoner = {
    val automaticReasoner = getAutomaticReasoner
    new PreferredReasoner(automaticReasoner)
  }


}
