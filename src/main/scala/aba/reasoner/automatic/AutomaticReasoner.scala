package aba.reasoner.automatic

import aba.framework.{Framework, Literal, Rule}
import aba.move.{DisputeAdvancement, TerminationCriteria}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF2, PF1}
import aba.move.TerminationCriteria.{TerminationCriteriaType, checkIfOver}
import aba.reasoner.automatic.AttackPreference.AttackPreferenceType
import aba.reasoner.automatic.OpponentsAttackStrategy.OpponentsAttackStrategyType
import aba.reasoner.automatic.RuleChoice.RuleChoiceType
import aba.reasoner.automatic.StatementChoice.StatementChoiceType
import aba.reasoner.automatic.TurnChoice.TurnChoiceType
import aba.reasoner.automatic.AssumptionChoice.AssumptionChoiceType
import aba.reasoner.{Argument, DisputeState, LiteralArgument, PotentialMove, RuleArgument}

import scala.annotation.tailrec

case class PerformedMove(moveType: MoveType, argument: Argument) {
  override def toString: String = s"$moveType: ${argument match {
    case LiteralArgument(lit) => lit.id
    case RuleArgument(rule) => rule.toString
  }}"
}


object DisputeStateAuto {
  def apply(potentialMove: PotentialMove, currentDStateAuto: DisputeStateAuto, increaseBranchingLevel: Boolean): DisputeStateAuto = {

    //
    val newArgument = potentialMove match {
      case PotentialMove(Some(ruleArgument), None, _, _, _, _) => ruleArgument
      case PotentialMove(None, Some(assumptionArgument), _, _, _, _) => assumptionArgument
    }
    val newPerformedMove = PerformedMove(potentialMove.moveType, newArgument)

    val branchingLevel = if (increaseBranchingLevel) currentDStateAuto.branchingLevel + 1 else currentDStateAuto.branchingLevel

    new DisputeStateAuto(potentialMove.perform(currentDStateAuto.dState), currentDStateAuto.ignoredCulpritCandidates, currentDStateAuto.ignoredProponentAssumptions, currentDStateAuto.performedArguments :+ newPerformedMove, branchingLevel)
  }
}


case class DisputeStateAuto(dState: DisputeState,
                            ignoredCulpritCandidates: Set[Literal],
                            ignoredProponentAssumptions: Set[Literal],
                            performedArguments: List[PerformedMove],
                            branchingLevel: Int = 0// TODO: temprary
                           ) {
  def performedMovesToString: List[String] =
    performedArguments.zipWithIndex.map { case (arg, index) => s"${index + 1}: [${arg.toString}]" }
}


class AutomaticReasoner(val turnChoice: TurnChoiceType,
                        val pStatementChoice: StatementChoiceType,
                        val oStatementChoice: StatementChoiceType,
                        val opponentsAttackStrategy: OpponentsAttackStrategyType,
                        val pAttackPreference: AttackPreferenceType,
                        val oAttackPreference: AttackPreferenceType,
                        val pRuleChoice: RuleChoiceType,
                        val oRuleChoice: RuleChoiceType,
                        val pAssumptionChoice: AssumptionChoiceType,
                        val oAssumptionChoice: AssumptionChoiceType,
                        val dfs: Boolean = false,
                        val putOpponentAtTheEndIfBFS: Boolean = false
                       ) {


  // TODO: define implicit conversion to dot comment which would do // + costam.mkString("// ")


  def settingsToString : List[String] = {
    List(s"turnChoice = $turnChoice",
       s"pStatementChoice = $pStatementChoice",
       s"oStatementChoice = $oStatementChoice",
       s"oppAttackStrategy = ${OpponentsAttackStrategy.OneAtATime}",
       s"pAttackPreference = $pAttackPreference",
       s"oAttackPreference = $oAttackPreference",
       s"pRuleChoice = $pRuleChoice",
       s"oRuleChoice = $oRuleChoice",
       s"pAssumptionChoice = $pAssumptionChoice",
       s"oAssumptionChoice = $oAssumptionChoice")
  }


  // TODO: this should also be moved from here maybe to the rule choice etc
  type TurnChoiceFunction = (Map[Literal, List[(Literal, MoveType, PotentialMove)]], Map[Literal, List[(Literal, MoveType, PotentialMove)]]) => Boolean
  type StatementChoiceFunction = (Set[Literal], Framework, List[PerformedMove]) => Literal
  type AttackPreferenceFunction =  (List[(Literal, MoveType, PotentialMove)], RuleChoiceType, AssumptionChoiceType, Framework, Set[Literal]) => List[PotentialMove]
  // TODO: this should ONLY be (rule, rule) => boolean
  // the other parameters should be added by some function currying or something
  type RuleChoiceFunction = (Framework, Set[Literal]) => (Rule, Rule) => Boolean
  type AssumptionChoiceFunction = Framework => (Literal, Literal) => Boolean





  private val assumptionChoiceMap: Map[AssumptionChoiceType, AssumptionChoiceFunction] = Map(
                                                                                                                // TODO: move this to other functions as well
    AssumptionChoice.MostContraries -> { framework: Framework => {
      case (lit1: Literal, lit2: Literal) => framework.contraries.count(_.assumption == lit1) > framework.contraries.count(_.assumption == lit2)
    }
    },
    AssumptionChoice.LeastContraries -> { framework: Framework => {
      case (lit1: Literal, lit2: Literal) => framework.contraries.count(_.assumption == lit1) < framework.contraries.count(_.assumption == lit2)
    }
    }
  )

  private val ruleChoiceMap: Map[RuleChoiceType, RuleChoiceFunction] = Map (
    RuleChoice.NewlyIntroducedStatementsMax ->
      { case (_, currentLiterals) => { case (rule1: Rule, rule2: Rule) => (rule1.statements diff currentLiterals).size > (rule2.statements diff currentLiterals).size } },
    RuleChoice.NewlyIntroducedStatementsMin ->
      { case (_, currentLiterals) => { case (rule1: Rule, rule2: Rule) => (rule1.statements diff currentLiterals).size < (rule2.statements diff currentLiterals).size } },
    RuleChoice.NewlyIntroducedAssumptionsMax ->
      { case (framework, currentLiterals) => { case (rule1: Rule, rule2: Rule) =>
        ((rule1.statements diff currentLiterals) intersect framework.assumptions).size > ((rule2.statements diff currentLiterals) intersect framework.assumptions).size } },
    RuleChoice.NewlyIntroducedAssumptionsMin ->
      { case (framework, currentLiterals) => { case (rule1: Rule, rule2: Rule) =>
        ((rule1.statements diff currentLiterals) intersect framework.assumptions).size < ((rule2.statements diff currentLiterals) intersect framework.assumptions).size } },
  )


  // true -> Proponent
  // false -> Opponent
  private val turnChoiceMap: Map[TurnChoiceType, TurnChoiceFunction] = Map (
    TurnChoice.Proponent -> { case (mapP, _) => mapP.nonEmpty },
    TurnChoice.Opponent -> { case (_, mapO) => mapO.nonEmpty },
    TurnChoice.LargestStatementSet -> { case (mapP, mapO) => mapP.size > mapO.size },
    TurnChoice.SmallestStatementSet -> { case (mapP, mapO) => mapP.size < mapO.size }
  )

  private val statementChoiceMap: Map[StatementChoiceType, StatementChoiceFunction] = Map (
    StatementChoice.Eager -> { case (statementsSet, framework, _) =>
      val assumptionsStatements = framework.assumptions intersect statementsSet
      if (assumptionsStatements.nonEmpty) assumptionsStatements.head else statementsSet.head
    },
    StatementChoice.Patient -> { case (statementSet, framework, _) =>
      val nonAssumptionsStatements = statementSet diff framework.assumptions
      if (nonAssumptionsStatements.nonEmpty) nonAssumptionsStatements.head else statementSet.head
    }, // be sure to filter the performed moves so that only the actual moves of a player are shown
    StatementChoice.Oldest -> { case (statementsSet, framework, performedMoves) =>
      performedMoves.collectFirst {
        case PerformedMove(_, RuleArgument(rule)) if (rule.body + rule.head).intersect(statementsSet).nonEmpty => rule.body + rule.head
        case PerformedMove(_, LiteralArgument(lit)) if statementsSet.contains(lit) => Set(lit)
      } match {
        case Some(set) => statementsSet.intersect(set).head
        case _ => statementsSet.head // no move was performed yet
      }
    },
    StatementChoice.Newest -> { case (statementsSet, framework, performedMoves) =>
    performedMoves.reverse.collectFirst {
      case PerformedMove(_, RuleArgument(rule)) if (rule.body + rule.head).intersect(statementsSet).nonEmpty => rule.body + rule.head
      case PerformedMove(_, LiteralArgument(lit)) if statementsSet.contains(lit) => Set(lit)
    } match {
      case Some(set) => statementsSet.intersect(set).head
      case _ => statementsSet.head
    }
  }

  )

  private val attackPreferenceMap: Map[AttackPreferenceType, AttackPreferenceFunction] = Map (
    AttackPreference.PreferRuleAttack -> { case (list, ruleChoiceType, assumptionChoiceType, framework, statements) =>
      val (ruleAttacks, assumptionAttacks) = list.partition(_._2.isRuleMove)

      // TODO: these thing should be somewhere in some function
      val ruleAttacksSorted = ruleAttacks.map(_._3).sortWith {
        case (PotentialMove(Some(RuleArgument(rule1)), None, _, PB2 | OB2, _, _), // TODO: i should have already added the function as a parameter instead. then i dont have to add framework and statements
        PotentialMove(Some(RuleArgument(rule2)), None, _, PB2 | OB2, _, _)) => ruleChoiceMap(ruleChoiceType)(framework, statements)(rule1, rule2)
      }

      val assumptionAttacksSorted = assumptionAttacks.map(_._3).sortWith {
        case (PotentialMove(None, Some(LiteralArgument(lit1)), _, PF2 | OF2, _, _),
          PotentialMove(None, Some(LiteralArgument(lit2)), _, PF2 | OF2, _, _)) => assumptionChoiceMap(assumptionChoiceType)(framework)(lit1, lit2)

      }
      ruleAttacksSorted ++ assumptionAttacksSorted

    },
    AttackPreference.PreferAssumptionAttack -> { case (list, ruleChoiceType, assumptionChoiceType, framework, statements) =>
      val (ruleAttacks, assumptionAttacks) = list.partition(_._2.isRuleMove)

      // TODO: these thing should be somewhere in some function
      val ruleAttacksSorted = ruleAttacks.map(_._3).sortWith {
        case (PotentialMove(Some(RuleArgument(rule1)), None, _, PB2 | OB2, _, _), // TODO: i should have already added the function as a parameter instead. then i dont have to add framework and statements
        PotentialMove(Some(RuleArgument(rule2)), None, _, PB2 | OB2, _, _)) => ruleChoiceMap(ruleChoiceType)(framework, statements)(rule1, rule2)
      }

      val assumptionAttacksSorted = assumptionAttacks.map(_._3).sortWith {
        case (PotentialMove(None, Some(LiteralArgument(lit1)), _, PF2 | OF2, _, _),
        PotentialMove(None, Some(LiteralArgument(lit2)), _, PF2 | OF2, _, _)) => assumptionChoiceMap(assumptionChoiceType)(framework)(lit1, lit2)
      }

      assumptionAttacksSorted ++ ruleAttacksSorted
    }
  )


  // TODO: here instead just pass the functions already maybe?
  def generateNewDisputeStates(implicit possibleMoves: Map[MoveType, Seq[PotentialMove]],
                               framework: Framework,
                               dStateAuto: DisputeStateAuto) : List[DisputeStateAuto] = {
    // returns list. 1st one will be the one that's the current one and the rest goes on the stack

    implicit val dState: DisputeState = dStateAuto.dState

    val pMovesTypes = Seq(PB1, OB2, OF2, PF1) // move types that belong to the proponent's turn
    val oMovesTypes = Seq(OB1, PB2) // move types that belong to the proponent's turn
    // PF2 sometimes belongto P, sometimes to O


    //val moves = possibleMoves.partition { case (moveType, _) => pMovesTypes.contains(moveType) }

    val defences = framework.defences
    val culpritCandidates = framework.culpritsCandidates

    val pStatements = framework.pLitArgs.map(_.lit)

    val bStatements = framework.bLitArgs.map(_.lit) diff pStatements

    // TODO: check if not empty
    val (oPF2Moves, pPF2Moves) = possibleMoves.filter(_._1 == PF2).values.flatten.partition {   // this filtering is in case PF2 map is empty
      case PotentialMove(None, Some(assArg), _, PF2, _, _) => framework.contrariesOf(culpritCandidates).contains(assArg.lit) // TODO: framework.contraryOf(assArg.lit)
    }

    val (pMovesWOPF2, oMovesWOPF2) = possibleMoves.filter(_._1 != PF2).partition {
      case (moveType, _) => pMovesTypes.contains(moveType)
    }

    val pMoves = pMovesWOPF2 + (PF2 -> pPF2Moves)
    val oMoves = oMovesWOPF2 + (PF2 -> oPF2Moves)



    val pMovesMap1 = pMoves.flatMap { case (moveType, potentialMovesSeq) => potentialMovesSeq.flatMap(potentialMove =>
      potentialMove match {
        // backward expansion of a proponent's statement
        case PotentialMove(Some(ruleArg), None, _, PB1, _, _) => List((ruleArg.rule.head, moveType, potentialMove))
        // this can be either attacking a defence OR attacking a culprit candidate
        case PotentialMove(Some(ruleArg), None, _, OB2, _, _) =>
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == ruleArg.rule.head).map(_.assumption) intersect framework.defences
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList

        case PotentialMove(None, Some(assumptionArg), _, OF2, _, _) =>
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == assumptionArg.lit).map(_.assumption) intersect framework.defences
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList

        // TODO:
        //  the forward moves from complete statements we can perform with no caution - assuming the framework is consistent
        //  anyways, current reasoner should handle it
        case PotentialMove(Some(ruleArg), None, _, PF1, _, _) => List((ruleArg.rule.head, moveType, potentialMove))

        // add only if non-ignored potential move
        case PotentialMove(None, Some(assumptionArg), _, PF2, _, _) =>
          if (dStateAuto.ignoredProponentAssumptions.contains(assumptionArg.lit)) Nil
          else List((assumptionArg.lit, moveType, potentialMove))
      }) }.toList.groupBy(_._1)

    val oMovesMap1 = oMoves.flatMap { case (moveType, potentialMovesSeq) => potentialMovesSeq.flatMap(potentialMove =>
      potentialMove match {
        case PotentialMove(Some(ruleArg), None, _, OB1, _, _) => List((ruleArg.rule.head, moveType, potentialMove))
        case PotentialMove(Some(ruleArg), None, _, PB2, _, _) =>
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == ruleArg.rule.head).map(_.assumption) intersect
              (culpritCandidates diff dStateAuto.ignoredCulpritCandidates)  // Ignored assumptions by proponent: he willnot attack those
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList

        case PotentialMove(None, Some(assumptionArg), _, PF2, _, _) =>
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == assumptionArg.lit).map(_.assumption) intersect
              (culpritCandidates diff dStateAuto.ignoredCulpritCandidates)
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList
      }) }.toList.groupBy(_._1)

    val isProponentChosen = turnChoiceMap(turnChoice)(pMovesMap1, oMovesMap1)

    val (proponentsPerformedMoves, opponentsPerformedMoves) = dStateAuto.performedArguments.partition{ case PerformedMove(moveType, _) => moveType.isProponentMove }


    if (isProponentChosen) {
      val chosenStatement = statementChoiceMap(pStatementChoice)(pMovesMap1.keySet, framework, proponentsPerformedMoves)
      // check if chosen statement is an assumption
      if (framework.assumptions.contains(chosenStatement)) {
        // if proponent chosen and statement is an assumption, then either it is a defence (it has to be attacked by opp)
        // OR an assumption to add to P (then we can ignore or add it to P)

        if (defences.contains(chosenStatement)) {
          // it is a defence; to be attacked by the opponent
          val movesToPerformSorted = attackPreferenceMap(oAttackPreference)(pMovesMap1(chosenStatement), oRuleChoice, oAssumptionChoice, framework, bStatements)
          // TODO: add option to attack using ALL possible options maybe
          val moveToPerform = movesToPerformSorted.head
          val newDStateAuto = DisputeStateAuto(moveToPerform, dStateAuto, increaseBranchingLevel = false)
          List(newDStateAuto)
        } else {
          // it is an assumption to be added to P
          val addingMove = pMovesMap1(chosenStatement).map(_._3).map(potMove => DisputeStateAuto(potMove, dStateAuto, increaseBranchingLevel = true)) // should be exactly one always anyways
          val ignoringMove = DisputeStateAuto(dState, dStateAuto.ignoredCulpritCandidates, dStateAuto.ignoredProponentAssumptions + chosenStatement, dStateAuto.performedArguments, dStateAuto.branchingLevel + 1)
          addingMove :+ ignoringMove
        }


      } else {
        if (pStatements.contains(chosenStatement)) {
          // backward extension by the proponent (PB1)
          val movesToPerformSorted = pMovesMap1(chosenStatement).filter(_._2 == PB1).map(_._3).sortWith {
            case (PotentialMove(Some(RuleArgument(rule1)), None, _, PB1, _, _),
            PotentialMove(Some(RuleArgument(rule2)), None, _, PB1, _, _)) =>
              ruleChoiceMap(pRuleChoice)(framework, pStatements)(rule1, rule2)
          }

          movesToPerformSorted.map(potMove => DisputeStateAuto(potMove, dStateAuto, increaseBranchingLevel = true))
        } else {
          // forward move PF1
          // a forward move, chosenStatement does not belong to P yet
          // we will not add ALL the rules, only a single one is sufficient to derive chosenStatement
          List(DisputeStateAuto(pMovesMap1(chosenStatement).filter(_._2 == PF1).map(_._3).head, dStateAuto, increaseBranchingLevel = false))
        }
      }
    } else {
      // opponent chosen
      val chosenStatement = statementChoiceMap(oStatementChoice)(oMovesMap1.keySet, framework, opponentsPerformedMoves)
      // check if chosen statement is an assumption
      if (framework.assumptions.contains(chosenStatement)) {
        // attack or ignore by proponent
        val ignoringMove = DisputeStateAuto(dState, dStateAuto.ignoredCulpritCandidates + chosenStatement, dStateAuto.ignoredProponentAssumptions, dStateAuto.performedArguments, dStateAuto.branchingLevel + 1)
        // attacking moves
        val movesToPerformSorted = attackPreferenceMap(pAttackPreference)(oMovesMap1(chosenStatement), pRuleChoice, pAssumptionChoice, framework, pStatements)
        val firstMove::restOfMoves = movesToPerformSorted.map(potMove => DisputeStateAuto(potMove, dStateAuto, increaseBranchingLevel = true))
        firstMove +: ignoringMove +: restOfMoves
      } else {
        // backward propagation by the opponent
        val movesToPerformSorted = oMovesMap1(chosenStatement).map(_._3).sortWith {
          case (PotentialMove(Some(RuleArgument(rule1)), None, _, OB1, _, _),
          PotentialMove(Some(RuleArgument(rule2)), None, _, OB1, _, _)) =>
            ruleChoiceMap(oRuleChoice)(framework, bStatements)(rule1, rule2) }

        val moveToPerform = movesToPerformSorted.head
        val newDStateAuto = DisputeStateAuto(moveToPerform, dStateAuto, increaseBranchingLevel = false)
        List(newDStateAuto)
      }
    }
  }


  @tailrec
  final def getNewIncompleteSuccessfulDSAndStackRec(stack: List[DisputeStateAuto],
                                              successfulDS: List[DisputeStateAuto],
                                              startTime: Option[Long] = None,
                                              iteration: Option[Int] = None)
                                             (implicit framework: Framework,
                                              onlyOne: Boolean,
                                              timeoutOpt: Option[Int],
                                              tCriteriaType: TerminationCriteriaType,
                                              dAdvancementType: DisputeAdvancementType): (List[DisputeStateAuto], List[DisputeStateAuto], Boolean, Double) = {
    // temporary
    val it: Int =  iteration match {
      case Some(i) => i
      case None => 0
    }


    // temporary
    val itInc = it + 1
    val itOp = Some(itInc)
    //println(s"Iteration: $itInc")


    val sTime = startTime match {
      case Some(t) => t
      case _ => System.nanoTime()
    }

    val duration = (System.nanoTime - sTime) / 1e9d

    timeoutOpt match {
      case Some(timeout) if (duration > timeout)  => (stack, successfulDS, true, duration)
      case _ =>

        if (stack.isEmpty || (onlyOne && successfulDS.nonEmpty)) (stack, successfulDS, false, duration)
        else {
          implicit val headDS :: restOfDisputeStates = stack // TODO: type annotation?
          implicit val headDState: DisputeState = headDS.dState
          implicit val possibleMoves: Map[MoveType, Seq[PotentialMove]] = DisputeAdvancement(dAdvancementType).getPossibleMoves

          // temporary
          if (headDS.performedArguments.isEmpty) {
            // println("Initial:")
            println(0 + ": " + headDS.dState.p.mkString("; ") + "\t" + s""" icc: { ${headDS.ignoredCulpritCandidates.mkString(";")} } ia: { ${headDS.ignoredProponentAssumptions.mkString(";")} }""")
          } else {
            val allPerformedMovesCount = headDS.performedArguments.size
            println(allPerformedMovesCount +":" + "\t" * headDS.branchingLevel + headDS.performedArguments.last.toString +  "\t" + s""" icc: { ${headDS.ignoredCulpritCandidates.mkString(";")} } ia: { ${headDS.ignoredProponentAssumptions.mkString(";")} }""")
          }

          TerminationCriteria.checkIfOver(tCriteriaType) match {
            case Some(true) =>
              println("\t" * headDS.branchingLevel + "SUCCESS!")
              getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS :+ headDS, Some(sTime), itOp)
            case Some(false) =>
              println("\t" * headDS.branchingLevel + "FAIL!")
              getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS, Some(sTime), itOp)
            case None =>

              // it is possible that the only PossibleMoves are to attack an ignored culprit candidate
              // OR PF1 moves with assumptions chosen as ignored
              if (possibleMoves.values.flatten.forall {
                // attack using a rule case
                case PotentialMove(Some(ruleArg), None, _, PB2, _, _) => framework.contrariesOf(headDS.ignoredCulpritCandidates).contains(ruleArg.rule.head)

                case PotentialMove(None, Some(assArg), _, PF2, _, _) =>
                  // attack using an assumption
                  if (framework.contrariesOf(framework.culpritsCandidates).contains(assArg.lit)) framework.contrariesOf(headDS.ignoredCulpritCandidates).contains(assArg.lit)
                  // add an assumption to P
                  else headDS.ignoredProponentAssumptions.contains(assArg.lit)
                //case PotentialMove(None, Some(assArg), _, PF2, _) if !framework.contrariesOf(framework.culpritsCandidates).contains(assArg.lit) => headDS.ignoredProponentAssumptions.contains(assArg.lit)
                case _ => false
              }) {
                println("\t" * headDS.branchingLevel + "FAIL! (only ignored moves left)")
                getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS, Some(sTime), itOp)
              } else {

                // ...
                val newDisputeStates = generateNewDisputeStates
                val disputeStates = if (dfs) newDisputeStates ++ restOfDisputeStates else restOfDisputeStates ++ newDisputeStates
                getNewIncompleteSuccessfulDSAndStackRec(disputeStates, successfulDS, Some(sTime), itOp)
              }
          }
        }
    }
  }
}
