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

  def proponentSeemsToBeWinning(implicit framework: Framework, dState: DisputeState): Boolean = {
    val unblockedCompletePlayedPiecesLit = framework.unblockedCompletePlayedPiecesB.collect { case litArg: LiteralArgument => litArg.lit }
    val completeArgsPLit = framework.completePiecesP.collect { case litArg: LiteralArgument => litArg.lit }

    val condition1 = (framework.contrariesOf(framework.defences) intersect unblockedCompletePlayedPiecesLit).isEmpty
    val condition2 = (framework.goals union framework.contrariesOf(framework.culprits)).subsetOf(completeArgsPLit)

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


    val (propCondBool, oppCondBool) = terminationCriteriaType match {
      case TA => (
        // prop condition
        proponentSeemsToBeWinning &&
          (dabOpponentMoves.forall(_._2.isEmpty) || dfOpponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty)),
        // opp condition
        opponentSeemsToBeWinning &&
          (dabProponentMoves.forall(_._2.isEmpty) || dfProponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty))
      )
      case TC => (
        // prop condition
        proponentSeemsToBeWinning && (framework.j -- framework.defences).isEmpty &&
          (dabOpponentMoves.forall(_._2.isEmpty) || dfOpponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty)),
        // opp condition
        (opponentSeemsToBeWinning || (framework.j -- framework.defences).nonEmpty) &&
          (dcProponentMoves.forall(_._2.isEmpty) || dfProponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty) )
      )
      case TS => (
        // prop condition
        proponentSeemsToBeWinning && ((framework.defences ++ framework.culprits) == framework.assumptions) &&
          (dabOpponentMoves.forall(_._2.isEmpty) || dfOpponentMoves.filter(_._1.isForwardMove).forall(_._2.isEmpty)),
        // opp condition
        (opponentSeemsToBeWinning || ((framework.defences ++ framework.culprits) != framework.assumptions)) &&
          dsProponentMoves.forall(_._2.isEmpty)
      )
    }

    if(propCondBool) Some(true)
    else if (oppCondBool) Some(false)
    else None
  }


  def seemsToBeWinningDebug(implicit dState: DisputeState, framework: Framework): Unit = {

    val unblockedCompletePlayedPiecesLit = framework.unblockedCompletePlayedPiecesB.collect { case litArg: LiteralArgument => litArg.lit }
    val completeArgsPLit = framework.completePiecesP.collect { case litArg: LiteralArgument => litArg.lit }

    val defencesContraries = framework.contrariesOf(framework.defences)
    val condition1 = (framework.contrariesOf(framework.defences) intersect unblockedCompletePlayedPiecesLit).isEmpty
    println(s"Condition 1: ${if (condition1) "TRUE" else "FALSE" }")
    println(s"(DefencesContraries INTERSECT UnblockedCompletePlayedPieces) is empty?")
    println(s"(${defencesContraries.mkString(", ")} INTERSECT ${unblockedCompletePlayedPiecesLit.mkString(", ")}) is empty?")


    val culpritContraries = framework.contrariesOf(framework.culprits)
    val condition2 = (framework.goals union culpritContraries).subsetOf(completeArgsPLit)
    println(s"Condition 2: ${if (condition2) "TRUE" else "FALSE" }")
    println(s"((Goals UNION CulpritContraries) SUBSETOF CompletePArgs)")
    println(s"(${framework.goals.mkString(", ")} UNION ${culpritContraries.mkString(", ")}) SUBSETOF ${completeArgsPLit.mkString(", ")})")

    val conditionComplete = (framework.j -- framework.defences).isEmpty
    println(s"Condition 'Complete': ${if (conditionComplete) "TRUE" else "FALSE" }")
    println(s"((J \\ Defences) is empty?)")
    println(s"({${framework.j.mkString(", ")}} \\ {${framework.defences.mkString(", ")}} is empty?)")
    println(s"Left: (${(framework.j -- framework.defences).mkString(", ")})")

    val conditionStable = (framework.defences ++ framework.culprits) == framework.assumptions
    println(s"Condition 'Stable': ${if (conditionStable) "TRUE" else "FALSE" }")
    println(s"((Defences UNION Culprits) == Assumptions)")
    println(s"Left: (${(framework.assumptions -- (framework.defences ++ framework.culprits)).mkString(", ")})")

  }
}