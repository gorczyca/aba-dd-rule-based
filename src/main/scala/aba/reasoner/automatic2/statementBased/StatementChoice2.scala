package aba.reasoner.automatic2.statementBased

import aba.framework.{Framework, Rule}


object StatementChoice2 extends Enumeration {

  type StatementChoiceType2 = Value

  val
    Eager, // e
    Patient, // p
    Newest, // n
    Oldest, // o
    LBCEager, // lbce
    LBCPatient, // lbcp
    GBCEager,   // gbce
    GBCPatient  // gbcp
  = Value


  def apply(statementChoiceType: StatementChoiceType2)(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String])
  : String = {

    statementChoiceType match {
      case Eager => statementEager
      case Patient => statementPatient
      case Newest => statementNewest
      case Oldest => statementOldest
      case LBCEager => statementLBCEager
      case LBCPatient => statementLBCPatient
      case GBCEager => statementGBCEager
      case GBCPatient => statementGBCPatient
    }
  }

  private def statementEager(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String = {
    val assumptions = statements intersect framework.assumptions
    if (assumptions.nonEmpty) assumptions.head else statements.head
  }

  private def statementPatient(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String = {
    val nonAssumptions = statements -- framework.assumptions
    if (nonAssumptions.nonEmpty) nonAssumptions.head else statements.head
  }

  private def statementNewest(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String =
    statements.toList.sortWith((s1, s2) => statementSequence.indexOf(s1) > statementSequence.indexOf(s2)).head

  private def statementOldest(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String =
    statements.toList.sortWith((s1, s2) => statementSequence.indexOf(s1) < statementSequence.indexOf(s2)).head

  private def statementLBCPatient(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String = {
    val nonAssumptions = statements -- framework.assumptions
    if (nonAssumptions.nonEmpty) {
      nonAssumptions.toList.sortWith((s1, s2) => nonAssumptionBranchingCoefficient(s1, remainingRules) < nonAssumptionBranchingCoefficient(s2, remainingRules)).head
    } else {
      // only assumptions
      statements.toList.sortWith((a1, a2) => assumptionBranchingCoefficient(a1, remainingRules) < assumptionBranchingCoefficient(a2, remainingRules)).head
    }
  }

  private def statementGBCPatient(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String = {
    val nonAssumptions = statements -- framework.assumptions
    if (nonAssumptions.nonEmpty) {
      nonAssumptions.toList.sortWith((s1, s2) => nonAssumptionBranchingCoefficient(s1, remainingRules) > nonAssumptionBranchingCoefficient(s2, remainingRules)).head
    } else {
      // only assumptions
      statements.toList.sortWith((a1, a2) => assumptionBranchingCoefficient(a1, remainingRules) > assumptionBranchingCoefficient(a2, remainingRules)).head
    }
  }

  private def statementLBCEager(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String = {
    val assumptions = statements intersect framework.assumptions
    if (assumptions.nonEmpty) {
      assumptions.toList.sortWith((s1, s2) => assumptionBranchingCoefficient(s1, remainingRules) < assumptionBranchingCoefficient(s2, remainingRules)).head
    } else {
      // only statements
      statements.toList.sortWith((a1, a2) => nonAssumptionBranchingCoefficient(a1, remainingRules) < nonAssumptionBranchingCoefficient(a2, remainingRules)).head
    }
  }

  private def statementGBCEager(implicit statements: Set[String], remainingRules: Set[Rule], framework: Framework, statementSequence: List[String]): String = {
    val assumptions = statements intersect framework.assumptions
    if (assumptions.nonEmpty) {
      assumptions.toList.sortWith((s1, s2) => assumptionBranchingCoefficient(s1, remainingRules) > assumptionBranchingCoefficient(s2, remainingRules)).head
    } else {
      // only statements
      statements.toList.sortWith((a1, a2) => nonAssumptionBranchingCoefficient(a1, remainingRules) > nonAssumptionBranchingCoefficient(a2, remainingRules)).head
    }
  }

  // helpers
  private def assumptionBranchingCoefficient(asm: String, remainingRules: Set[Rule])(implicit framework: Framework): Int = {
    val asmContraries = framework.contrariesOf(asm)
    remainingRules.count {
      case Rule(_, head, _) => asmContraries.contains(head)
    }
  }

  private def nonAssumptionBranchingCoefficient(st: String, remainingRules: Set[Rule]): Int = remainingRules.count {
    case Rule(_, head, _) => head == st
  }


}
