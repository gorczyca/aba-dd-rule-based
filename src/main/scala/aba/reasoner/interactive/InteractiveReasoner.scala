package aba.reasoner.interactive

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.{Move, OB1Move, OB2Move, TerminationCriteria}
import aba.move.Move.{OB1, OB2, OF1, PB1, PB2, PF1, PF2, OF2}
import aba.move.TerminationCriteria.TerminationCriteriaType
import aba.reasoner.argumentBased2.DisputeStateAB2
import aba.reasoner.{DisputeState, PotentialMove2, PotentialRuleMove}
import aba.reasoner.automatic2.DisputeStateAuto2

import scala.annotation.tailrec
import scala.util.Random

object OpponentDisputeStateAuto {
  def apply(potentialMove: PotentialMove2)(implicit currentDStateAuto: OpponentDisputeStateAuto, framework: Framework): OpponentDisputeStateAuto = {

    val remainingOppRules = potentialMove match {
      case ruleMove: PotentialRuleMove => InteractiveReasoner.filterOpponentRules(Set(ruleMove.rule.head), currentDStateAuto.remainingOpponentRules)
      case _ => currentDStateAuto.remainingOpponentRules
    }

    implicit val dState: DisputeState = currentDStateAuto.dState

    OpponentDisputeStateAuto(
      dState = potentialMove.perform,
      performedMoves = currentDStateAuto.performedMoves :+ potentialMove,
      remainingOpponentRules = remainingOppRules
    )
  }
}




case class OpponentDisputeStateAuto(dState: DisputeState,
                                    performedMoves: List[PotentialMove2],
                                    remainingOpponentRules: Set[Rule])

object InteractiveReasoner {
  def default(dAdvancementType: DisputeAdvancementType,
              tCriteriaType: TerminationCriteriaType): InteractiveReasoner = {
    InteractiveReasoner(
      propCount = 1,
      oppCount = 1,
      oppArgCount = 1,
      dAdvancementType = dAdvancementType,
      tCriteriaType = tCriteriaType
    )
  }

  def filterOpponentRules(usedRulesHeads: Set[String], remainingRules: Set[Rule]): Set[Rule] = remainingRules.filter { case Rule(_, head, _)  => usedRulesHeads.contains(head) }
}


case class InteractiveReasoner(
                         // propCount
                         propCount: Int,
                         // oppCount
                         oppCount: Int,
                         oppArgCount: Int,
                         dAdvancementType: DisputeAdvancementType,
                         tCriteriaType: TerminationCriteriaType
                         // oppArgsPerStatement
                         // propPreferAttack
                         // oppPreferAttack
                         // TODO: set some options here
                         ) {

  //

  //

  @tailrec
  final def getNewProponentArgs(implicit dSAuto: DisputeStateAuto2, framework: Framework, remainingCountOpt: Option[Int] = None): DisputeStateAuto2 = {

    val remainingCount = remainingCountOpt match {
      case Some(r) => r
      case None => propCount
    }

    if (remainingCount == 0) return dSAuto

    // TODO: options
    val dState = dSAuto.dState

    val statementsToExpand = dState.pPlayedUnexpandedStatements -- framework.assumptions
    val culpritCandidatesContraries = dState.culpritCandidatesContraries

    val statementsTogether = statementsToExpand ++ culpritCandidatesContraries

    if (statementsTogether.isEmpty) dSAuto
    else {
      // TODO: instead simulate some choice
      val chosenStmt = Random.shuffle(statementsTogether).head
      val initialDSAuto = dSAuto.copy(proponentsStatementsToProveOpt = Some(Set(chosenStmt)))
      val completeArgsForStmt = propFindAllCompleteArgsRec(chosenStmt, List(initialDSAuto), Nil)

      if (completeArgsForStmt.isEmpty) dSAuto // TODO: here also allow for incomplete?
      else {
        // todo: simulate some choice
//        val chosenArg = completeArgsForStmt.head
        val chosenArg = Random.shuffle(completeArgsForStmt).head
        getNewProponentArgs(chosenArg, framework, Some(remainingCount - 1))

      }
    }
  }

  private val possibleProponentMoveTypes = Seq(PB1, PB2, PF1, PF2) // TODO: should I allow for the forward moves here?

  @tailrec
  private def propFindAllCompleteArgsRec(statement: String, stack: List[DisputeStateAuto2], successfulCompleteArgs: List[DisputeStateAuto2])
                                    (implicit framework: Framework): List[DisputeStateAuto2] = {

    stack match {
      case head::rest =>
        implicit val headDS: DisputeState = head.dState
        if (headDS.pPlayedCompleteStatements.contains(statement)) propFindAllCompleteArgsRec(statement, rest, successfulCompleteArgs :+ head)
        else {
                                                                                // advancement type is irrelevant here
          val possibleMoves = possibleProponentMoveTypes
            .flatMap(Move(_).isPossible(dAdvancementType).map(move => move.asInstanceOf[PotentialRuleMove]))
            .filter { move => head.proponentsStatementsToProveOpt.get.contains(move.rule.head) } // take only those that are relevant
            .groupBy(_.moveType)

          if (possibleMoves.isEmpty) propFindAllCompleteArgsRec(statement, rest, successfulCompleteArgs)
          else {
            // TODO: choose one move type (should be enough)
            val nextStates = possibleMoves.head._2.map { move =>
              implicit val currentDSAuto: DisputeStateAuto2 = head
              val newStatements = (head.proponentsStatementsToProveOpt.get - move.rule.head) ++ (move.rule.body -- headDS.pPlayedCompleteStatements)
              DisputeStateAuto2(move, newStatements)
            }.toList

            propFindAllCompleteArgsRec(statement, nextStates ++ rest, successfulCompleteArgs)
          }


        }
      case Nil => successfulCompleteArgs
    }

  }

  // opponent
  private val possibleOpponentMoveTypes = Seq(OB1, OF1, OB2, OF2) // TODO: make a switch here

  @tailrec
  private def getRelevantRules(statementsToSupport: Set[String], rules: Set[Rule], relevantRules: Set[Rule]): Set[Rule] = {

    val (newRelevantRules, remainingRules) = rules.partition { case Rule(_, head, _) => statementsToSupport.contains(head) }

    if (newRelevantRules.isEmpty) relevantRules
    else {
      val newStatements = remainingRules.flatMap(_.body)
      getRelevantRules(newStatements, remainingRules, relevantRules ++ newRelevantRules)
    }
  }

  final def getNewOpponentArgs(implicit dSAuto: DisputeStateAuto2, framework: Framework): DisputeStateAuto2 = {

    // TODO: options
    implicit val dState: DisputeState = dSAuto.dState

                                    // TODO: here actual pass the advancement type
    val statementsToExpand = OB1Move.validRuleHeads(dAdvancementType)(framework, dSAuto.dState)
    val contrariesAttackingDefences = OB2Move.validRuleHeads(dAdvancementType)(framework, dSAuto.dState)

    val statementsTogether = statementsToExpand ++ contrariesAttackingDefences

    if (statementsTogether.isEmpty) dSAuto
    else {
      // TODO: put choosing inside this function
      findAllCompleteArgumentsForStatementOpponent(statementsTogether)
    }

  }

  @tailrec
  private def constructDSBasedOnRules(dSAuto: DisputeStateAuto2, rules: Set[Rule])(implicit framework: Framework): DisputeStateAuto2 = {

    implicit val dState = dSAuto.dState

    val allPossibleMoves = possibleOpponentMoveTypes
      // TODO: here add actual advancement type
      .flatMap(Move(_).isPossible(dAdvancementType))

    val possibleMoves = possibleOpponentMoveTypes
      // TODO: here add actual advancement type
      .flatMap(Move(_).isPossible(dAdvancementType).map(move => move.asInstanceOf[PotentialRuleMove]))
      .filter { move => rules.contains(move.rule) } // take only those that are relevant

    if (possibleMoves.isEmpty) dSAuto
    else {
      val moveToPerform = possibleMoves.head
      val newDState = DisputeStateAuto2(moveToPerform)(dSAuto, framework)
      constructDSBasedOnRules(newDState, rules)
    }

  }

  private def findAllCompleteArgumentsForStatementOpponent(statements: Set[String]) // TODO: here set of statements
                                                        (implicit dSAuto: DisputeStateAuto2,
                                                          framework: Framework): DisputeStateAuto2 = {

    implicit val dState: DisputeState = dSAuto.dState

    val relevantRules = dState.bRules.filter { case Rule(_, _, body) => (body intersect dState.culprits).isEmpty } ++ getRelevantRules(statements, dState.bRemainingNonBlockedRules, Set.empty)
    // TODO: this filtering here

    val statementArgumentMap = statements.map(stmt => {
      //val stmtArgsRuleSet = DisputeStateAB2.create_arguments(Set(stmt), relevantRules).filter(arg => !arg.isCircular && arg.isComplete && !arg.isConflicted).map(_.rulesUsed)

      // first take nonComplete / nonConflicted / nonCircular
      //val (complete, incomplete) = DisputeStateAB2.create_arguments(Set(stmt), relevantRules).partition(arg => !arg.isCircular && arg.isComplete && !arg.isConflicted)


      val stmtArgsRuleSet = DisputeStateAB2.create_arguments(Set(stmt), relevantRules).map(arg => {
        val isComplete = !arg.isCircular && arg.isComplete && !arg.isConflicted
        (isComplete, arg.rulesUsed)
      }).filterNot { case (_, rules) => rules.subsetOf(dState.bRules) }
      // TODO: filter

      //val filteredRuleSets = stmtArgsRuleSet.filterNot(set => set.subsetOf(dState.bRules))
      (stmt, stmtArgsRuleSet)
    }).filter(_._2.nonEmpty)


    val completeArgs = statementArgumentMap.map { case (stmt, set) =>
      (stmt, set.filter(_._1))
    }.filter(_._2.nonEmpty)

    val incompleteArgs = statementArgumentMap.map { case (stmt, set) =>
      (stmt, set.filter(!_._1))
    }.filter(_._2.nonEmpty)


//    val targetSet = if (completeArgs.nonEmpty) completeArgs else incompleteArgs
    val targetSet = completeArgs


    val targetSet2 = targetSet.map { case (stmt, set) => {
      (stmt, set.map(_._2))
    } }.toMap

    // TODO: this should be done easier
    val randomKeys = Random.shuffle(targetSet2.keys.toList).take(oppCount)
    val chosenStatementMap = targetSet2.filter { case (stmt, _) => randomKeys.contains(stmt) }
    val chosenArgs = chosenStatementMap.map { case (k, v) => (k, Random.shuffle(v).take(oppArgCount)) }
    val rulesFlattened = chosenArgs.flatMap(_._2).flatten.toSet
    constructDSBasedOnRules(dSAuto, rulesFlattened)


    //val int = 123
    //val c = shuffle(targetSet2).toMap // TODO: tu cos sie zwalilo
//
//    val x = 1
//
//    // TODO: choosing here.
//    val chosenStatements = shuffle(targetSet2).take(oppCount).toMap
//    // TODO: choosing here too
//    val chosenArgs = chosenStatements.map{ case (k, v) => (k, Random.shuffle(v).take(oppArgCount)) }
//
//    val rulesFlattened = chosenArgs.flatMap(_._2).flatten.toSet
//
//    constructDSBasedOnRules(dSAuto, rulesFlattened)
  }

  def canMove(implicit framework: Framework,
              dState: DisputeState,
              tCriteria: TerminationCriteriaType,
              dAdvancement: DisputeAdvancementType): (Boolean, Boolean) = {

    val newDSAuto = new DisputeStateAuto2(dState, Set.empty, Set.empty, Nil, tCriteria, dAdvancement, None)

    val newStateAutoP = getNewProponentArgs(newDSAuto, framework, Some(1))
    val newStateAutoO = getNewOpponentArgs(newDSAuto, framework)

    val possibleProp = newStateAutoP.performedMoves.nonEmpty
    val possibleOpp = newStateAutoO.performedMoves.nonEmpty

    (possibleProp, possibleOpp)

  }

  def checkIfOver(implicit framework: Framework,
                  dState: DisputeState): Option[Boolean] = {

    val (possibleProp, possibleOpp) = canMove(framework, dState, tCriteriaType, dAdvancementType)

    (TerminationCriteria.proponentSeemsToBeWinning(framework, dState, tCriteriaType), possibleProp, possibleOpp) match {
      case (true, _, false) => Some(true)
      case (false, false, _) => Some(false)
      case _ => None
    }
  }


}
