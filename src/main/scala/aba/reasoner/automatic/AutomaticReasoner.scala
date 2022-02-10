package aba.reasoner.automatic

import aba.framework.{Framework, Literal, Rule}
import aba.move.{DisputeAdvancement, OB1Move, OB2Move, OF2Move, PB1Move, PB2Move, PF1Move, PF2Move, TerminationCriteria}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.{TerminationCriteriaType, checkIfOver}
import aba.reasoner.automatic.AttackPreference.AttackPreferenceType
import aba.reasoner.automatic.OpponentsAttackStrategy.OpponentsAttackStrategyType
import aba.reasoner.automatic.RuleChoice.RuleChoiceType
import aba.reasoner.automatic.StatementChoice.StatementChoiceType
import aba.reasoner.automatic.TurnChoice.TurnChoiceType
import aba.reasoner.automatic.AssumptionChoice.AssumptionChoiceType
import aba.reasoner.{Argument, DisputeState, LiteralArgument, PotentialAssumptionMove, PotentialMove, PotentialMove2, PotentialRuleMove, RuleArgument}

import scala.annotation.tailrec

trait PerformedMove2{
  val moveType: MoveType
  def repr: String
  override def toString: String = f"$moveType: $repr"
}

case class PerformedRuleMove(override val moveType: MoveType, rule: Rule) extends PerformedMove2 {
  override def repr: String = rule.toString
}

case class PerformedAssumptionMove(override val moveType: MoveType, assumption: String) extends PerformedMove2 {
  override def repr: String = assumption
}


case class PerformedMove(moveType: MoveType, argument: Argument) {
  override def toString: String = s"$moveType: ${argument match {
    case LiteralArgument(lit) => lit.id
    case RuleArgument(rule) => rule.toString
  }}"
}


object DisputeStateAuto {
  def apply(potentialMove: PotentialMove2, currentDStateAuto: DisputeStateAuto, increaseBranchingLevel: Boolean)(implicit framework: Framework): DisputeStateAuto = {

    val newPerformedMove = potentialMove.toPerformedMove

    val branchingLevel = if (increaseBranchingLevel) currentDStateAuto.branchingLevel + 1 else currentDStateAuto.branchingLevel

    val dState = potentialMove.perform(currentDStateAuto.dState, framework)

    new DisputeStateAuto(dState, currentDStateAuto.ignoredCulpritCandidates, currentDStateAuto.ignoredProponentAssumptions, currentDStateAuto.performedArguments :+ newPerformedMove, branchingLevel)
  }
}


case class DisputeStateAuto(dState: DisputeState,
                            ignoredCulpritCandidates: Set[String],
                            ignoredProponentAssumptions: Set[String],
                            performedArguments: List[PerformedMove2],
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
  type TurnChoiceFunction = (Map[String, List[(String, MoveType, PotentialMove2)]], Map[String, List[(String, MoveType, PotentialMove2)]]) => Boolean
  type StatementChoiceFunction = (Set[String], Framework, List[PerformedMove2]) => String
  type AttackPreferenceFunction =  (List[(String, MoveType, PotentialMove2)], RuleChoiceType, AssumptionChoiceType, Framework, Set[String]) => List[PotentialMove2]
  // TODO: this should ONLY be (rule, rule) => boolean
  // the other parameters should be added by some function currying or something
  type RuleChoiceFunction = (Framework, Set[String]) => (Rule, Rule) => Boolean
  type AssumptionChoiceFunction = Framework => (String, String) => Boolean





  private val assumptionChoiceMap: Map[AssumptionChoiceType, AssumptionChoiceFunction] = Map(
                                                                                                                // TODO: move this to other functions as well
    AssumptionChoice.MostContraries -> { framework: Framework => {
      case (asm1: String, asm2: String) => framework.contraries.count(_.assumption == asm1) > framework.contraries.count(_.assumption == asm2)
    }
    },
    AssumptionChoice.LeastContraries -> { framework: Framework => {
      case (asm1: String, asm2: String) => framework.contraries.count(_.assumption == asm1) < framework.contraries.count(_.assumption == asm2)
    }
    }
  )

  private val ruleChoiceMap: Map[RuleChoiceType, RuleChoiceFunction] = Map (
    RuleChoice.NewlyIntroducedStatementsMax ->
      { case (_, currentStatements) => { case (rule1: Rule, rule2: Rule) => (rule1.statements diff currentStatements).size > (rule2.statements diff currentStatements).size } },
    RuleChoice.NewlyIntroducedStatementsMin ->
      { case (_, currentStatement) => { case (rule1: Rule, rule2: Rule) => (rule1.statements diff currentStatement).size < (rule2.statements diff currentStatement).size } },
    RuleChoice.NewlyIntroducedAssumptionsMax ->
      { case (framework, currentStatements) => { case (rule1: Rule, rule2: Rule) =>
        ((rule1.statements diff currentStatements) intersect framework.assumptions).size > ((rule2.statements diff currentStatements) intersect framework.assumptions).size } },
    RuleChoice.NewlyIntroducedAssumptionsMin ->
      { case (framework, currentStatements) => { case (rule1: Rule, rule2: Rule) =>
        ((rule1.statements diff currentStatements) intersect framework.assumptions).size < ((rule2.statements diff currentStatements) intersect framework.assumptions).size } },
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
    StatementChoice.Oldest -> { case (statementsSet, _, performedMoves) =>
      performedMoves.collectFirst {
        case PerformedRuleMove(_, rule) if (rule.body + rule.head).intersect(statementsSet).nonEmpty => rule.body + rule.head // TODO: makes no sense to make the if here. there is no other option
        case PerformedAssumptionMove(_, asm) if statementsSet.contains(asm) => Set(asm)
      } match {
        case Some(set) => statementsSet.intersect(set).head
        case _ => statementsSet.head // no move was performed yet
      }
    },
    StatementChoice.Newest -> { case (statementsSet, _, performedMoves) =>
    performedMoves.reverse.collectFirst {
      case PerformedRuleMove(_, rule) if (rule.body + rule.head).intersect(statementsSet).nonEmpty => rule.body + rule.head
      case PerformedAssumptionMove(_, asm) if statementsSet.contains(asm) => Set(asm)
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
        case (PotentialRuleMove(rule1), PotentialRuleMove(rule2)) => ruleChoiceMap(ruleChoiceType)(framework, statements)(rule1, rule2)
      }

      val assumptionAttacksSorted = assumptionAttacks.map(_._3).sortWith {
        case (PotentialAssumptionMove(asm1), PotentialAssumptionMove(asm2)) =>  assumptionChoiceMap(assumptionChoiceType)(framework)(asm1, asm2)

      }
      ruleAttacksSorted ++ assumptionAttacksSorted

    },
    AttackPreference.PreferAssumptionAttack -> { case (list, ruleChoiceType, assumptionChoiceType, framework, statements) =>
      val (ruleAttacks, assumptionAttacks) = list.partition(_._2.isRuleMove)

      // TODO: these thing should be somewhere in some function
      val ruleAttacksSorted = ruleAttacks.map(_._3).sortWith { // TODO: duplicated code
        case (PotentialRuleMove(rule1), PotentialRuleMove(rule2)) => ruleChoiceMap(ruleChoiceType)(framework, statements)(rule1, rule2)
      }

      val assumptionAttacksSorted = assumptionAttacks.map(_._3).sortWith {
        case (PotentialAssumptionMove(asm1), PotentialAssumptionMove(asm2)) => assumptionChoiceMap(assumptionChoiceType)(framework)(asm1, asm2)
      }

      assumptionAttacksSorted ++ ruleAttacksSorted
    }
  )


  // TODO: here instead just pass the functions already maybe?
  def generateNewDisputeStates(implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                               framework: Framework,
                               dStateAuto: DisputeStateAuto) : List[DisputeStateAuto] = {
    // returns list. 1st one will be the one that's the current one and the rest goes on the stack

    implicit val dState: DisputeState = dStateAuto.dState

    val pMovesTypes = Seq(PB1, OB2, OF2, PF1) // move types that belong to the proponent's turn
    val oMovesTypes = Seq(OB1, PB2) // move types that belong to the proponent's turn
    // PF2 sometimes belongto P, sometimes to O


    //val moves = possibleMoves.partition { case (moveType, _) => pMovesTypes.contains(moveType) }

    val defences = dState.defences
    val culpritCandidates = dState.culpritCandidates

    val pStatements = dState.pStatements


    // TODO:
    val bStatements = dState.oStatements  -- pStatements // framework.bLitArgs.map(_.lit) diff pStatements

    // TODO: check if not empty
    val (oPF2Moves, pPF2Moves) = possibleMoves.filter(_._1 == PF2).values.flatten.partition {   // this filtering is in case PF2 map is empty
      case PF2Move(asm, _, _) => dState.culpritCandidatesContraries.contains(asm)
    }

    val (pMovesWOPF2, oMovesWOPF2) = possibleMoves.filter(_._1 != PF2).partition {
      case (moveType, _) => pMovesTypes.contains(moveType)
    }

    val pMoves = pMovesWOPF2 + (PF2 -> pPF2Moves)
    val oMoves = oMovesWOPF2 + (PF2 -> oPF2Moves)



    val pMovesMap1 = pMoves.flatMap { case (moveType, potentialMovesSeq) => potentialMovesSeq.flatMap(potentialMove =>
      potentialMove match {
        // backward expansion of a proponent's statement
        case PB1Move(rule, _, _) => List((rule.head, moveType, potentialMove))
        // this can be either attacking a defence OR attacking a culprit candidate
        case OB2Move(rule, _, _) =>
          // TODO: here reuse "attacking" property
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == rule.head).map(_.assumption) intersect dState.defences
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList

        case OF2Move(asm, _, _) =>
          // TODO: here reuse as well
          // todo, traits with attacking etc?
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == asm).map(_.assumption) intersect dState.defences
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList

        // TODO:
        //  the forward moves from complete statements we can perform with no caution - assuming the framework is consistent
        //  anyways, current reasoner should handle it
        case PF1Move(rule, _, _) => List((rule.head, moveType, potentialMove))

        // add only if non-ignored potential move
        case PF2Move(asm, _, _) =>
          if (dStateAuto.ignoredProponentAssumptions.contains(asm)) Nil
          else List((asm, moveType, potentialMove))
      }) }.toList.groupBy(_._1)

    val oMovesMap1 = oMoves.flatMap { case (moveType, potentialMovesSeq) => potentialMovesSeq.flatMap(potentialMove =>
      potentialMove match {
        case OB1Move(rule, _, _) => List((rule.head, moveType, potentialMove))
        case PB2Move(rule, _, _) =>
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == rule.head).map(_.assumption) intersect
              (culpritCandidates diff dStateAuto.ignoredCulpritCandidates)  // Ignored assumptions by proponent: he willnot attack those
          assumptionsToAttack.map(ass => (ass, moveType, potentialMove)).toList

        case PF2Move(asm, _, _) =>
          val assumptionsToAttack =
            framework.contraries.filter(_.contrary == asm).map(_.assumption) intersect
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
          implicit val possibleMoves: Map[MoveType, Seq[PotentialMove2]] = DisputeAdvancement(dAdvancementType).getPossibleMoves

          // temporary
          if (headDS.performedArguments.isEmpty) {
            // println("Initial:")
            println(0 + ": " + (headDState.pStatements ++ headDState.pRules).mkString("; ") + "\t" + s""" icc: { ${headDS.ignoredCulpritCandidates.mkString(";")} } ia: { ${headDS.ignoredProponentAssumptions.mkString(";")} }""")
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
                case PB2Move(rule, _, _) => framework.contrariesOf(headDS.ignoredCulpritCandidates).contains(rule.head)

                case PF2Move(asm, _, _) =>
                  // attack using an assumption
                  if (headDState.culpritCandidatesContraries.contains(asm)) framework.contrariesOf(headDS.ignoredCulpritCandidates).contains(asm)
                  // add an assumption to P
                  else headDS.ignoredProponentAssumptions.contains(asm)
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
