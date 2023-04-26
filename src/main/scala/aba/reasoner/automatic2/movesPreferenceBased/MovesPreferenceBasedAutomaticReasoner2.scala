package aba.reasoner.automatic2.movesPreferenceBased

import aba.framework.Framework
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.TerminationCriteriaType
import aba.reasoner.{DisputeState, PotentialAssumptionMove, PotentialMove2, PotentialRuleMove}
import aba.reasoner.automatic2.RuleChoice2.{BodyMin, RuleChoiceType2}
import aba.reasoner.automatic2.movesPreferenceBased.RuleHeadChoice.RuleHeadChoiceType
import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2.MovesGrouped
import aba.reasoner.automatic2.{AutomaticReasoner2, DisputeStateAuto2, RuleChoice2}

// TODO:

object MovesPreferenceBasedAutomaticReasoner2 {

  def default(terminationCriteriaType: TerminationCriteriaType,
              disputeAdvancementType: DisputeAdvancementType): MovesPreferenceBasedAutomaticReasoner2 = {

    new MovesPreferenceBasedAutomaticReasoner2(
      dfs = true, // dfs
      tCriteriaType =  terminationCriteriaType,
      dAdvancementType = disputeAdvancementType,
      startWithAdmissible = true,
//      startWithAdmissible = false,
//      preferenceOrdering = Seq(PF1, PF2, OF1, PB2, OB2, OF2, OB1, PB1),
      preferenceOrdering = Seq(PF1, PB2, PF2, PB1, OB2, OF2, OB1), // s1
//      preferenceOrdering = Seq(PF1, OB2, OF2, OB1, PB1, PB2, PF2, OF1), // s2

//      preferenceOrdering = Seq(OB2, OF2, OB1, PB1, PB2, PF2, OF1, PF1),
//      pRuleHeadChoice = RuleHeadChoice.MostRules,
      pRuleHeadChoice = RuleHeadChoice.LeastRules,
//      oRuleHeadChoice = RuleHeadChoice.MostRules,
      oRuleHeadChoice = RuleHeadChoice.LeastRules,
//      pRuleChoice = RuleChoice2.BodyMax,
      pRuleChoice = RuleChoice2.NewlyIntroducedStatementsMin,
//      oRuleChoice = RuleChoice2.BodyMax
      oRuleChoice = RuleChoice2.NewlyIntroducedStatementsMin
    )
  }
}



case class MovesPreferenceBasedAutomaticReasoner2(dfs: Boolean,
                                                  tCriteriaType: TerminationCriteriaType,
                                                  dAdvancementType: DisputeAdvancementType,
                                                  startWithAdmissible: Boolean,
                                                  preferenceOrdering: Seq[MoveType],
                                                  pRuleHeadChoice: RuleHeadChoiceType,
                                                  oRuleHeadChoice: RuleHeadChoiceType,
                                                  pRuleChoice: RuleChoiceType2,
                                                  oRuleChoice: RuleChoiceType2
                                                 ) extends AutomaticReasoner2(dfs, dAdvancementType, tCriteriaType, startWithAdmissible) {
  override def toString: String = {
    s"Search type:\t\t${if (dfs) "DFS" else "BFS"}\n" +
      s"Start with admissible:\t${booleanToString(startWithAdmissible)}\n" +
      s"Preference ordering:\t[${preferenceOrdering.mkString(", ")}]\n" +
      s"Prop. rule head choice:\t${pRuleHeadChoice}\n" +
      s"Opp. rule head choice:\t${oRuleHeadChoice}\n" +
      s"Prop. rule choice:\t${pRuleChoice}\n" +
      s"Opp. rule choice:\t${oRuleChoice}\n" +
      //s"\n" +
      s"Advancement type:\t${dAdvancementType}\n" +
      s"Termination criteria:\t${tCriteriaType}"
  }

  private def booleanToString(bool: Boolean): String = if (bool) "YES" else "NO"

  override protected def generateNewDisputeStates(implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]], framework: Framework, dStateAuto: DisputeStateAuto2): List[DisputeStateAuto2] = {

    // get moves by preferred move type
    val possibleMoveTypes = possibleMoves.keys.map(mType =>
      (mType, preferenceOrdering.indexOf(mType))
    )

    val chosenMoveType = possibleMoveTypes.toList.minBy(_._2)._1
    val movesToChooseFrom = possibleMoves(chosenMoveType)

    // defaults
    implicit val dState: DisputeState = dStateAuto.dState
    implicit val terminationCriteria: TerminationCriteriaType = tCriteriaType


    chosenMoveType  match {
      case PB1 => pb1(movesToChooseFrom)
      case PB2 => pb2(movesToChooseFrom)
      case PF1 => pf1(movesToChooseFrom)
      case PF2 => pf2(movesToChooseFrom)
      case OB1 => ob1(movesToChooseFrom)
      case OB2 => ob2(movesToChooseFrom)
      case OF1 => of1(movesToChooseFrom)
      case OF2 => of2(movesToChooseFrom)
    }
  }

  private def pb1(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {

    implicit val pStatements: Set[String] = dState.pStatements
    implicit val chosenRuleMoves: MovesGrouped = RuleHeadChoice(pRuleHeadChoice)(moves)
    RuleChoice2(pRuleChoice).map(DisputeStateAuto2(_))

  }


  private def pb2(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {

    implicit val pStatements: Set[String] = dState.pStatements
    implicit val chosenRuleMoves: MovesGrouped = RuleHeadChoice(pRuleHeadChoice)(moves)

    // attacked assumption
    val attackedAssumptions = chosenRuleMoves.head match {
      // TODO: a hotfix, do not add them all, but just the ones attacked by head
      case r: PotentialRuleMove => framework.contraries.filter(_.contrary == r.rule.head).map(_.assumption)
        // r.attacking match {
        // case Some(s) => s
        //}
    }

    val pb2Moves = RuleChoice2(pRuleChoice).map(DisputeStateAuto2(_))
    val ignoringMove = dStateAuto.copy(ignoredCulpritCandidates = dStateAuto.ignoredCulpritCandidates ++ attackedAssumptions)

    ignoringMove +: pb2Moves

  }


  private def pf1(moves: Seq[PotentialMove2])(implicit  framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {
    // just perform the first that is possible
    val chosenRuleMoves = RuleHeadChoice(pRuleHeadChoice)(moves)
    DisputeStateAuto2(chosenRuleMoves.head) :: Nil

  }


  private def pf2(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {

    val chosenMove = moves.head
    // check if attacking
    chosenMove.attacking match {
      case Some(attackedAssumptions) =>
        // either attack them or ignore them
          val ignoringMove = dStateAuto.copy(ignoredCulpritCandidates = dStateAuto.ignoredCulpritCandidates ++ attackedAssumptions)
          // ignore the attacked culprit candidates or attack them with the move
          ignoringMove :: DisputeStateAuto2(chosenMove) :: Nil

      case None =>
        // either take the assumption or ignore it
        val ignoringMove = dStateAuto.copy(ignoredProponentAssumptions = dStateAuto.ignoredProponentAssumptions + chosenMove.asInstanceOf[PotentialAssumptionMove].assumption)
        // ignore it or take it to P
        ignoringMove :: DisputeStateAuto2(chosenMove) :: Nil
    }
  }


  private def ob1(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {

    implicit val oStatements: Set[String] = dState.oStatements
    implicit val chosenRuleMoves: MovesGrouped = RuleHeadChoice(oRuleHeadChoice)(moves)
    val sortedRuleMoves = RuleChoice2(oRuleChoice)
    DisputeStateAuto2(sortedRuleMoves.head) :: Nil

  }

  private def ob2(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {

    implicit val oStatements: Set[String] = dState.oStatements
    implicit val chosenRuleMoves: MovesGrouped = RuleHeadChoice(oRuleHeadChoice)(moves)
    val sortedRuleMoves = RuleChoice2(oRuleChoice)
    DisputeStateAuto2(sortedRuleMoves.head) :: Nil

  }

  private def of1(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {
    // just perform the first that is possible
    val chosenRuleMoves = RuleHeadChoice(oRuleHeadChoice)(moves)
    DisputeStateAuto2(chosenRuleMoves.head) :: Nil
  }

  private def of2(moves: Seq[PotentialMove2])(implicit framework: Framework, dState: DisputeState, dStateAuto: DisputeStateAuto2, tCriteria: TerminationCriteriaType): List[DisputeStateAuto2] = {
    // just take any of2 move
    DisputeStateAuto2(moves.head) :: Nil
  }
}
