package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, PotentialMove}
import aba.move.DisputeAdvancement.{DAB, DC, DF, DS}
import aba.move.Move.MoveType
import aba.move.TerminationCriteria.{TC, TS}

object TerminationCriteria extends Enumeration {
  type TerminationCriteriaType = Value

  val
   TA,
   TC,
   TS
  = Value

  implicit def fromString(terminationTypeString: String): TerminationCriteriaType = values.find(_.toString.equalsIgnoreCase(terminationTypeString)) match {
    case Some(value) => value
    case None => throw new Exception(s"No termination criteria type: $terminationTypeString")
  }

  // TODO change to val
  def proponentSeemsToBeWinning(implicit framework: Framework, dState: DisputeState): Boolean = {
    val culpritContraries = framework.contrariesOf(dState.culprits)

    val condition1 = (dState.defenceContraries intersect dState.bUnblockedCompletePlayedStatements).isEmpty
    val condition2 = (framework.goals union culpritContraries).subsetOf(dState.pPlayedCompleteStatements)

    condition1 && condition2
  }

  def opponentSeemsToBeWinning(implicit framework: Framework, dState: DisputeState): Boolean = !proponentSeemsToBeWinning

  def checkIfOver(terminationCriteriaType: TerminationCriteriaType)(implicit framework: Framework, dState: DisputeState): Option[Boolean] = {

//    val (dabOpponentMoves, dabProponentMoves) = DisputeAdvancement(DAB).getPossibleMoves.partition(_._1.isOpponentsMove)
//    val (dfOpponentMoves, dfProponentMoves) = DisputeAdvancement(DF).getPossibleMoves.partition(_._1.isOpponentsMove)
//    val (dcOpponentMoves, dcProponentMoves) = DisputeAdvancement(DC).getPossibleMoves.partition(_._1.isOpponentsMove)
//    val (_, dsProponentMoves) = DisputeAdvancement(DS).getPossibleMoves.partition(_._1.isOpponentsMove)

    val Seq((dabOpponentMoves, dabProponentMoves),
            (dfOpponentMoves, dfProponentMoves),
            (_, dcProponentMoves),
            (_, dsProponentMoves))
      = Seq(DisputeAdvancement(DAB),
            DisputeAdvancement(DF),
            DisputeAdvancement(DC),
            DisputeAdvancement(DS))
      .map(_.getPossibleMoves.partition(_._1.isOpponentsMove))

    val propSeemsWin = proponentSeemsToBeWinning
    val oppSeemsWin = opponentSeemsToBeWinning

    val additionalTCCondition = (dState.currentlyDefendedAssumptions -- dState.defences).isEmpty
    val additionalTSCondition = framework.assumptions == (dState.defences ++ dState.culprits)

    val (propCondBool, oppCondBool) = terminationCriteriaType match {
      case TA => (
        // prop condition
        propSeemsWin &&
          (dabOpponentMoves.forall(_._2.isEmpty) || dfOpponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty)),
        // opp condition
        oppSeemsWin &&
          (dabProponentMoves.forall(_._2.isEmpty) || dfProponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty))
      )
      case TC => (
        // prop condition
        propSeemsWin && additionalTCCondition &&
          (dabOpponentMoves.forall(_._2.isEmpty) || dfOpponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty)),
        // opp condition
        oppSeemsWin || !additionalTCCondition &&
          (dcProponentMoves.forall(_._2.isEmpty) || dfProponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty) )
      )
      case TS => (
        // prop condition
        propSeemsWin && additionalTSCondition &&
          (dabOpponentMoves.forall(_._2.isEmpty) || dfOpponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty)),
        // opp condition
        oppSeemsWin || !additionalTSCondition &&
          dsProponentMoves.forall(_._2.isEmpty)
      )
    }

    if(propCondBool) Some(true)
    else if (oppCondBool) Some(false)
    else None
  }


//  def seemsToBeWinningDebug(implicit dState: DisputeState, framework: Framework): Unit = {
//
//    val unblockedCompletePlayedPiecesLit = framework.unblockedCompletePlayedPiecesB.collect { case litArg: LiteralArgument => litArg.lit }
//    val completeArgsPLit = framework.completePiecesP.collect { case litArg: LiteralArgument => litArg.lit }
//
//    val defencesContraries = framework.contrariesOf(framework.defences)
//    val condition1 = (framework.contrariesOf(framework.defences) intersect unblockedCompletePlayedPiecesLit).isEmpty
//    println(s"Condition 1: ${if (condition1) "TRUE" else "FALSE" }")
//    println(s"(DefencesContraries INTERSECT UnblockedCompletePlayedPieces) is empty?")
//    println(s"(${defencesContraries.mkString(", ")} INTERSECT ${unblockedCompletePlayedPiecesLit.mkString(", ")}) is empty?")
//
//
//    val culpritContraries = framework.contrariesOf(framework.culprits)
//    val condition2 = (framework.goals union culpritContraries).subsetOf(completeArgsPLit)
//    println(s"Condition 2: ${if (condition2) "TRUE" else "FALSE" }")
//    println(s"((Goals UNION CulpritContraries) SUBSETOF CompletePArgs)")
//    println(s"(${framework.goals.mkString(", ")} UNION ${culpritContraries.mkString(", ")}) SUBSETOF ${completeArgsPLit.mkString(", ")})")
//
//    val conditionComplete = (framework.j -- framework.defences).isEmpty
//    println(s"Condition 'Complete': ${if (conditionComplete) "TRUE" else "FALSE" }")
//    println(s"((J \\ Defences) is empty?)")
//    println(s"({${framework.j.mkString(", ")}} \\ {${framework.defences.mkString(", ")}} is empty?)")
//    println(s"Left: (${(framework.j -- framework.defences).mkString(", ")})")
//
//    val conditionStable = (framework.defences ++ framework.culprits) == framework.assumptions
//    println(s"Condition 'Stable': ${if (conditionStable) "TRUE" else "FALSE" }")
//    println(s"((Defences UNION Culprits) == Assumptions)")
//    println(s"Left: (${(framework.assumptions -- (framework.defences ++ framework.culprits)).mkString(", ")})")
//
//  }
}