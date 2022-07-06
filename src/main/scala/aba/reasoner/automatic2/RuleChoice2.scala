package aba.reasoner.automatic2

import aba.framework.{Framework, Rule}
import aba.move.TerminationCriteria.{TC, TS, TerminationCriteriaType}
import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2.MovesGrouped
import aba.reasoner.{DisputeState, PotentialMove2, PotentialRuleMove}

object RuleChoice2 extends Enumeration {

  type RuleChoiceType2 = Value

  val
  BodyMax, // bmin
  BodyMin, // bmax
  NewlyIntroducedStatementsMin, // smin
  NewlyIntroducedStatementsMax, // smax
  NewlyIntroducedAssumptionsMin, // amin
  NewlyIntroducedAssumptionsMax, // amax
  LookAhead1Step // l1s
  = Value

  def apply(ruleChoiceType: RuleChoiceType2)(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] = {

    ruleChoiceType match {
      case BodyMin => ruleBodyMin
      case BodyMax => ruleBodyMax
      case NewlyIntroducedStatementsMin => ruleNewlyIntroducedStatementsMin
      case NewlyIntroducedStatementsMax => ruleNewlyIntroducedStatementsMax
      case NewlyIntroducedAssumptionsMin => ruleNewlyIntroducedAssumptionsMin
      case NewlyIntroducedAssumptionsMax => ruleNewlyIntroducedAssumptionsMax
      case LookAhead1Step => ruleLookAhead1Step
    }

  }


  private def ruleBodyMin(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] = {
    ruleMoves.toList.sortBy { case m: PotentialRuleMove => m.rule.body.size }
  }

  private def ruleBodyMax(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] = {
    ruleMoves.toList.sortBy { case m: PotentialRuleMove => m.rule.body.size }.reverse
  }

  private def ruleNewlyIntroducedStatementsMin(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] =
    ruleMoves.toList.sortWith {
      case (m1: PotentialRuleMove, m2: PotentialRuleMove) => ruleNewlyIntroducedStatementsSize(m1.rule, statements) < ruleNewlyIntroducedStatementsSize(m2.rule, statements)
    }

  private def ruleNewlyIntroducedStatementsMax(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] =
    ruleMoves.toList.sortWith {
      case (m1: PotentialRuleMove, m2: PotentialRuleMove) => ruleNewlyIntroducedStatementsSize(m1.rule, statements) > ruleNewlyIntroducedStatementsSize(m2.rule, statements)
    }

  private def ruleNewlyIntroducedAssumptionsMin(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] =
    ruleMoves.toList.sortWith {
      case (m1: PotentialRuleMove, m2: PotentialRuleMove) => ruleNewlyIntroducedAssumptionsSize(m1.rule, statements, framework.assumptions) < ruleNewlyIntroducedAssumptionsSize(m2.rule, statements, framework.assumptions)
    }

  private def ruleNewlyIntroducedAssumptionsMax(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] =
    ruleMoves.toList.sortWith {
      case (m1: PotentialRuleMove, m2: PotentialRuleMove) => ruleNewlyIntroducedAssumptionsSize(m1.rule, statements, framework.assumptions) > ruleNewlyIntroducedAssumptionsSize(m2.rule, statements, framework.assumptions)
    }

  private def ruleLookAhead1Step(implicit ruleMoves: MovesGrouped, statements: Set[String], dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): List[PotentialMove2] =
    ruleMoves.toList.sortWith((m1, m2) => ruleLookAhead1StepSize(m1) < ruleLookAhead1StepSize(m2))


  // helpers
  private def ruleNewlyIntroducedStatementsSize(rule: Rule, statements: Set[String]): Int = (rule.statements -- statements).size


  private def ruleNewlyIntroducedAssumptionsSize(rule: Rule, statements: Set[String], assumptionsSet: Set[String]): Int = ((rule.statements -- statements) intersect assumptionsSet).size


  private def ruleLookAhead1StepSize(move: PotentialMove2)
                                    (implicit dState: DisputeState, framework: Framework, terminationCriteriaType: TerminationCriteriaType): Int = {

    val newDState = move.perform

    val unblockedDefenceContrariesSize = (newDState.defenceContraries intersect newDState.bUnblockedCompletePlayedStatements).size

    val culpritContraries = framework.contrariesOf(newDState.culprits) // TODO: keep this in dState
    val incompleteStatementsForGoalsAndCulpritContrariesSize =
      ((framework.goals union culpritContraries) -- newDState.pPlayedCompleteStatements).size

    val additionalSize = terminationCriteriaType match {
      case TC => (newDState.currentlyDefendedAssumptions -- newDState.defences).size
      case TS => (framework.assumptions -- (newDState.defences ++ newDState.culprits)).size
      case _ => 0
    }

    unblockedDefenceContrariesSize + incompleteStatementsForGoalsAndCulpritContrariesSize + additionalSize
  }
}
