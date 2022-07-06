package aba.reasoner

import aba.framework.{Contrary, Framework, Rule}

import scala.annotation.tailrec


object DisputeState {

  def initial(implicit framework: Framework, initialAssumptions: Option[Set[String]] = None): DisputeState = {


    val goals = initialAssumptions match {
      case Some(ass) => ass ++ framework.goals
      case None => framework.goals
    }

    // TODO: contraries
    // TODO: self conflicting assumptions

    val (initialDefences, nonAssumptionsGoals) = goals.partition(goal => framework.assumptions.contains(goal))
    val initialCulprits = framework.contraries.filter(ctr => goals.contains(ctr.contrary)).map(_.assumption)
    val initialDefencesContraries = framework.contrariesOf(initialDefences)

    // TODO: if initialDefences contain constraints, throw!!!

    // TODO: but if anything here is not empty, we should end the ds already
    // TODO: here if (initialDefences intersect initialCulprits).nonEmpty then throw
    val goalsAttackingDefences = goals.intersect(framework.contrariesOf(initialDefences))
    val unblockedGoalsAttackingDefences = goalsAttackingDefences -- initialCulprits

    val assumptionsAttackingDefences = framework.contraries.filter { case Contrary(assumption, _) => initialDefences.contains(assumption) }.map(_.contrary) intersect framework.assumptions

    val currentlyDefendedAssumptions = framework.assumptions -- (initialCulprits ++ framework.selfContradictingAssumptions ++ assumptionsAttackingDefences)
    val currentlyDefendedAssumptionsContraries = framework.contrariesOf(currentlyDefendedAssumptions)

    //val unblockedGoalsAttackingDefencedAssumptions = (currentlyDefendedAssumptionsContraries intersect goals) -- initialCulprits

    // TODO: should always be empty
    val newCulpritCandidates = goalsAttackingDefences intersect framework.assumptions
    val newCulpritCandidatesContraries = framework.contrariesOf(newCulpritCandidates)

    val remainingNonBlockedRules = framework.rules.filter {
      case Rule(_, _, body) => (body intersect initialCulprits).isEmpty
    }

    val selfContradictingRules = framework.rules.filter {
      case r@Rule(_, _, body) => (r.statements intersect framework.contrariesOf(body)).nonEmpty
    }

    val initialPRemainingNonBlockedRules = remainingNonBlockedRules -- selfContradictingRules

    val pRemainingNonBlockedRules = DisputeState.calculatePRemainingNonBlockedRules(initialPRemainingNonBlockedRules, Set.empty, initialCulprits, initialDefencesContraries, framework.constraints)

    val pStatementsFollowingFromPCompleteStatements = pRemainingNonBlockedRules.filter {
      case Rule(_, head, body) => body.subsetOf(initialDefences)
    }.map(_.head)

    val newBFullyExpandedStatements = goals.filter(goal => !remainingNonBlockedRules.exists {
      case Rule(_, head, _) => head == goal
    })

    val newBPlayedBlockedStatements = (newBFullyExpandedStatements -- framework.assumptions) ++ initialCulprits

    DisputeState(
      pStatements = goals,
      pRules = Set.empty,
      oStatements = Set.empty,
      oRules = Set.empty,
      defences = initialDefences,
      culprits = initialCulprits,
      pRemainingNonBlockedRules = pRemainingNonBlockedRules,
      bRemainingNonBlockedRules = remainingNonBlockedRules,
      pPlayedUnexpandedStatements = nonAssumptionsGoals,
      bFullyExpandedStatements = newBFullyExpandedStatements, // TODO: should assumptions be here?
      bPlayedBlockedStatements = newBPlayedBlockedStatements,
      bPlayedBlockedRules = Set.empty,
      pPlayedCompleteStatements = initialDefences,
      pPlayedCompleteRules = Set.empty,
      bUnblockedCompletePlayedStatements = initialDefences,
      bUnblockedCompletePlayedRules = Set.empty,
      bUnblockedStatementsSupportingDefenceContraries = unblockedGoalsAttackingDefences, // TODO: should normally always be empty
      bUnblockedRulesSupportingDefenceContraries = Set.empty,
      bUnblockedStatementsSupportingDefendedAssumptionsContraries = Set.empty,
      bUnblockedRulesSupportingDefendedAssumptionsContraries = Set.empty,
      culpritCandidates = goalsAttackingDefences intersect framework.assumptions, // TODO: should normally always be empty
      currentlyDefendedAssumptions = currentlyDefendedAssumptions,
      defenceContraries = initialDefencesContraries,
      culpritCandidatesContraries = newCulpritCandidatesContraries,
      currentlyDefendedAssumptionsContraries = currentlyDefendedAssumptionsContraries,
      pStatementsFollowingFromPCompleteStatements = pStatementsFollowingFromPCompleteStatements
    )
  }

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
      val newSupportingRulesBodiesFlattened = newSupportingRules.flatMap(_.body) // TODO: it should be just fine, though according to the definition we should only take non-blocked
      val newSupportingStatements = bRemainingStatements intersect newSupportingRulesBodiesFlattened
      val newRemainingStatements = bRemainingStatements -- newSupportingStatements
      calculateBUnblockedPiecesSupportingStatementsInSet(bSupportingStatements ++ newSupportingStatements, bSupportingRules ++ newSupportingRules, newRemainingStatements, newRemainingRules)
    }
  }


  // functions used to progress dispute derivation

  def calculatePPlayedUnexpandedStatements(pStatements: Set[String], pRules: Set[Rule]): Set[String] =
    pStatements -- pRules.map(_.head)
  def calculatePRemainingNonBlockedRules(pRemainingNonBlockedRules: Set[Rule], pRules: Set[Rule], culprits: Set[String], defencesContraries: Set[String], constraints: Set[String]): Set[Rule] =
    pRemainingNonBlockedRules.filter {
      case r@Rule(_, head, body) => !pRules.map(_.head).contains(head) &&
        (body intersect culprits).isEmpty &&
        (r.statements intersect defencesContraries).isEmpty && // TODO: constraints here
        (r.statements intersect constraints).isEmpty
    }
  def calculateBRemainingNonBlockedRules(bRemainingNonBlockedRules: Set[Rule], bRules: Set[Rule], culprits: Set[String]): Set[Rule] =
    (bRemainingNonBlockedRules -- bRules).filter {
      case Rule(_, _, body) => (body intersect culprits).isEmpty
    }
  def calculateBFullyExpandedStatements(bRemainingNonBlockedRules: Set[Rule], bStatements: Set[String]): Set[String] = {
    bStatements -- bRemainingNonBlockedRules.map(_.head)
  } // TODO: here optimization possible
  def calculateNonCulpritsPlayedAssumptions(bStatements: Set[String], assumptions: Set[String], culprits: Set[String]): Set[String] =
    (bStatements intersect assumptions) -- culprits



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
                         // for complete semantics
                         pStatementsFollowingFromPCompleteStatements: Set[String]
                       ) {

  val bRules: Set[Rule] = pRules ++ oRules
  val bStatements: Set[String] = pStatements ++ oStatements

  override def toString: String = {

    val pCompStatements = pPlayedCompleteStatements.map(stmt => s"*${stmt}")
    val pCompRules = pPlayedCompleteRules.map(rule => s"*${rule}")

    val oCompStatements = (bUnblockedCompletePlayedStatements -- pStatements).map(stmt => s"*${stmt}")
    val oCompRules = (bUnblockedCompletePlayedRules -- pRules).map(rule => s"*${rule}")

    val oBlockedStatements = bPlayedBlockedStatements.map(stmt => s"-${stmt}")
    val oBlockedRules = bPlayedBlockedRules.map(rule => s"-${rule}")

    ///////////////

    val remainingOStatements = oStatements -- (pStatements ++ bUnblockedCompletePlayedStatements ++ bPlayedBlockedStatements)
    val remainingORules = (oRules -- (pRules ++ bUnblockedCompletePlayedRules ++ bPlayedBlockedRules)).map(_.toString)

    val remainingPStatements = pStatements -- pPlayedCompleteStatements
    val remainingPRules = pRules -- pPlayedCompleteRules

    s"P:\n\t${(pCompRules ++ pCompStatements ++ remainingPStatements ++ remainingPRules).mkString("; ")}\nB\\P:\n\t${(oCompRules ++ oCompStatements ++ oBlockedRules ++ oBlockedStatements ++ remainingORules ++ remainingOStatements).mkString("; ")}\nD:\n\t${defences.mkString(",")}\nC:\n\t${culprits.mkString(",")}"
  }
}
