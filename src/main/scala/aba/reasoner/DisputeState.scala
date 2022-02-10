package aba.reasoner

import aba.framework.Rule
import aba.move.Move.MoveType

import scala.annotation.tailrec


object DisputeState {
  // this factory method was created because the class constructor has execute "this" as the first statement
  //@deprecated
  //def apply(move: MoveType, t: Set[Argument], argument: Argument)(implicit previousState: DisputeState): DisputeState = {
  //  if (move.isProponentMove) new DisputeState(Set.empty, Set.empty, Set.empty, Set.empty, previousState.id + 1, Some(move), previousState.p ++ t, previousState.b ++ t, Some(argument))
  //  else new DisputeState(Set.empty, Set.empty, Set.empty, Set.empty, previousState.id + 1, Some(move), previousState.p, previousState.b ++ t, Some(argument))
  //}

  //@deprecated
  //def apply(goals: Set[String]): DisputeState = {
  //  new DisputeState(goals, Set.empty, Set.empty, Set.empty)
  //}

  // TODO: in the end this will just be the main constructor
  //def apply(pStatements: Set[String], pRules: Set[Rule], oStatements: Set[String], oRules: Set[Rule]): DisputeState = {
  //  new DisputeState(pStatements, pRules, oStatements, oRules)
  //}

}

case class DisputeState(
                         // basic things
                         pStatements: Set[String],
                         pRules: Set[Rule],
                         oStatements: Set[String],
                         oRules: Set[Rule],
                         // from the definitions
                         defences: Set[String],
                         culprits: Set[String],
                         pRemainingNonBlockedRules: Set[Rule],
                         bRemainingNonBlockedRules: Set[Rule],
                         pPlayedUnexpandedStatements: Set[String],
                         bFullyExpandedStatements: Set[String],
                         // every <sth>pieces gets converted into <sth>Rules and <sth>Statements
                         bPlayedBlockedStatements: Set[String],
                         bPlayedBlockedRules: Set[Rule],
                         pPlayedCompleteStatements: Set[String],
                         pPlayedCompleteRules: Set[Rule],
                         bUnblockedCompletePlayedStatements: Set[String],
                         bUnblockedCompletePlayedRules: Set[Rule],
                         bUnblockedStatementsSupportingDefenceContraries: Set[String],
                         bUnblockedRulesSupportingDefenceContraries: Set[Rule],
                         bUnblockedStatementsSupportingDefendedAssumptionsContraries: Set[String],
                         bUnblockedRulesSupportingDefendedAssumptionsContraries: Set[Rule],
                         culpritCandidates: Set[String],
                         currentlyDefendedAssumptions: Set[String],
                         // not for easier computation
                         defenceContraries: Set[String], // TODO: this should be calculated in Apply method when passing implicitly framework
                         culpritCandidatesContraries: Set[String],
                         currentlyDefendedAssumptionsContraries: Set[String],
                       ) {

  val bRules: Set[Rule] = pRules ++ oRules
  val bStatements: Set[String] = pStatements ++ oStatements


  // TODO: companion
  // TODO: optimization here is rather impossible
  @tailrec
  final def calculateBPlayedBlockedPiecesRec(blockedStatements: Set[String],
                                             blockedRules: Set[Rule],
                                             bRemainingStatementsToCheck: Set[String],
                                             bRemainingRulesToCheck: Set[Rule]): (Set[String], Set[Rule]) = {

    val (newBlockedRules, newRemainingRules) = bRemainingRulesToCheck.partition {
      case Rule(_, _, body) => (body intersect blockedStatements).nonEmpty
    }

    if (newBlockedRules.isEmpty) (blockedStatements, blockedRules)
    else {
      val (newBlockedStatements, newRemainingStatements) = bRemainingStatementsToCheck.partition(st => !newRemainingRules.exists(rule => rule.head == st))
      calculateBPlayedBlockedPiecesRec(blockedStatements ++ newBlockedStatements, blockedRules ++ newBlockedRules, newRemainingStatements, newRemainingRules)
    }
  }

  // TODO: companion
  @tailrec
  final def calculatePPlayedCompletePiecesRec(pCompleteStatements: Set[String],
                                              pCompleteRules: Set[Rule],
                                              pRemainingRulesToCheck: Set[Rule]): (Set[String], Set[Rule]) = {

    val (newCompleteRules, newRemainingRules) = pRemainingRulesToCheck.partition {
      case Rule(_, _, body) => body.subsetOf(pCompleteStatements)
    }

    if (newCompleteRules.isEmpty) (pCompleteStatements, pCompleteRules)
    else {
      val newCompleteStatements = newCompleteRules.map(_.head)
      calculatePPlayedCompletePiecesRec(pCompleteStatements ++ newCompleteStatements, pCompleteRules ++ newCompleteRules, newRemainingRules)
    }
  }


  // TODO: companion
  @tailrec
  final def calculateBUnblockedCompletePiecesRec(bUnblockedCompleteStatements: Set[String],
                                                 bUnblockedCompleteRules: Set[Rule],
                                                 bRemainingStatementsToCheck: Set[String],
                                                 bRemainingRulesToCheck: Set[Rule]): (Set[String], Set[Rule]) = {

    val (newUnblockedCompleteRules, newRemainingRules) = bRemainingRulesToCheck.partition {
      case Rule(_, _, body) => body.subsetOf(bUnblockedCompleteStatements)
    }

    if (newUnblockedCompleteRules.isEmpty) (bUnblockedCompleteStatements, bUnblockedCompleteRules)
    else {
      val (newBUnblockedCompleteStatements, newRemainingStatements) = bRemainingStatementsToCheck.partition(st => newUnblockedCompleteRules.exists(rule => rule.head == st))
      calculateBUnblockedCompletePiecesRec(bUnblockedCompleteStatements ++ newBUnblockedCompleteStatements, bUnblockedCompleteRules ++ newUnblockedCompleteRules, newRemainingStatements, newRemainingRules)
    }
  }


  // TODO: companion
  @tailrec
  final def calculateBUnblockedPiecesSupportingStatementsInSet(bSupportingStatements: Set[String],
                                                               bSupportingRules: Set[Rule],
                                                               bRemainingStatements: Set[String],
                                                               bRemainingRules: Set[Rule]): (Set[String], Set[Rule]) = {
    val (newSupportingRules, newRemainingRules) = bRemainingRules.partition {
      case Rule(_, head, _) => bSupportingStatements.contains(head)
    }

    if (newSupportingRules.isEmpty) (bSupportingStatements, bSupportingRules)
    else {
      val newSupportingRulesBodiesFlattened = newRemainingRules.flatMap(_.body)
      val newSupportingStatements = bRemainingStatements intersect newSupportingRulesBodiesFlattened
      val newRemainingStatements = bRemainingStatements -- newSupportingStatements
      calculateBUnblockedPiecesSupportingStatementsInSet(bSupportingStatements ++ newSupportingStatements, bSupportingRules ++ newSupportingRules, newRemainingStatements, newRemainingRules)
    }
  }



  override def toString: String = s"P: ${(pStatements ++ pRules).mkString("; ")}\nB\\P: ${(oStatements ++ oRules).mkString("; ")}"

//  @deprecated
//  def sequenceElement: String = s"$id. ${ move match {
//    case Some(value) => s"$value: ${ argument match {
//      case Some(arg) => s"$arg"
//      case _ => ""
//    }}"
//    case _ => "(init)"
//  }}"
}
