package aba.reasoner.structured.views

import aba.framework.Framework
import aba.move.Move.{OB2, OF2, PB2, PF2}
import aba.move.{OB2Move, OF2Move, PB2Move, PF2Move}
import aba.reasoner.argumentBased2.{ArgumentTree, DisputeStateAB2}
import aba.reasoner.structured.{ArgumentsToChooseFrom, StatementsToChooseFrom, StatementsWithinArgumentToChooseFrom, StructuredReasoner}
import aba.reasoner.{DisputeState, PotentialMove2}
import interface.ProgramState
import aba.reasoner.structured.StructuredReasoner.{digitRegex, indicesRegex, zippedToString}
import interface.InputProcessorInterface.getUserInput
import interface.dotConverters.ABRepresentationInterface.generateABRepresentation

import scala.annotation.tailrec
import scala.util.matching.Regex

object OpponentAttacking extends View {

  @tailrec
  override def goalView(implicit state: ProgramState): ProgramState = {

    implicit val framework: Framework = state.framework
    val dState = state.currentDState

    val pGoalsCulpritCandidates = (framework.contrariesOf(dState.culprits) intersect dState.pStatements) union framework.goals
    val argumentsForGoalsAndCulpritCandidates = DisputeStateAB2.create_arguments(pGoalsCulpritCandidates, dState.pRules)

    // TODO: take only the maximal ones
    val maximalPropArgs = argumentsForGoalsAndCulpritCandidates.filter(arg => !(argumentsForGoalsAndCulpritCandidates - arg).exists(argBigger => arg.rulesUsed.subsetOf(argBigger.rulesUsed)))

    val argumentsDefencesMap = maximalPropArgs.map(arg =>
      (arg,  (arg.rulesUsed.flatMap(_.statements) + arg.root.statement) intersect dState.defences)
    )

    def movesAttackingDefence(d: String): Seq[PotentialMove2] = {
      val ob2Of2Moves = state.possibleMoves.filter {
        case (OB2 | OF2, _) => true
        case _ => false
      }.values.flatten

      ob2Of2Moves.filter {
        case OB2Move(_, Some(attackedAssumptions)) => attackedAssumptions.contains(d)
        case OF2Move(_, Some(attackedAssumptions)) => attackedAssumptions.contains(d)
      }.toSeq
    }


    val argumentsDefencesMovesMap = argumentsDefencesMap.map {
      case (argTree, defences) => {
        val defencesMap = defences.map(d => (d, movesAttackingDefence(d))).filter{
          case (_, moves) => moves.nonEmpty
        }
        (argTree, defencesMap)
      }
    }.filter {
      case (_, defMap) => defMap.nonEmpty
    }.toMap

    val headArgumentsMap = argumentsDefencesMovesMap.map{
      case (argTree, defMap) =>
        (argTree.root.statement, (argTree, defMap))
    }

    val heads = headArgumentsMap.keys
    val headsZipped = heads.zipWithIndex

    println("Goal view:")
    println(zippedToString(headsZipped))

    generateABRepresentation(highlightedData = Some(StatementsToChooseFrom(heads.toSet, proponent = true)))

    // TODO: all of that should be in some interface
    val read = getUserInput
    read match {
      case "b" => StructuredReasoner.run
      case "" =>
        argumentView(argumentsDefencesMovesMap)
      case "q" => state

      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val headsMatching = headsZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        val argumentsDefencesMovesMapMatching = headArgumentsMap.filter {
          case (head, _) => headsMatching.contains(head)
        }.values.toMap

        argumentView(argumentsDefencesMovesMapMatching)

      case _ =>
        println("Invalid input.")
        goalView
    }
  }


  def argumentView(arguments: Map[ArgumentTree, Set[(String, Seq[PotentialMove2])]])(implicit state: ProgramState): ProgramState = {

    val argsZipped = arguments.keys.zipWithIndex

    println("Argument view:")
    println(zippedToString(argsZipped))

    generateABRepresentation(highlightedData = Some(ArgumentsToChooseFrom(arguments.keySet, proponent = true)))

    getUserInput match {
      case "" =>
        statementsView(arguments.keySet)(arguments)
      case "b" => goalView
      case "q" => state // exit
      case argsIndices if indicesRegex.matches(argsIndices) =>
        val indicesSet = argsIndices.split(",").map(_.toInt).toSet
        val argsMatching = argsZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        statementsView(argsMatching)(arguments)

      case _ =>
        println("Invalid input.")
        argumentView(arguments)
    }
  }

  @tailrec
  def statementsView(arguments: Set[ArgumentTree])(backtracking: Map[ArgumentTree, Set[(String, Seq[PotentialMove2])]])(implicit programState: ProgramState): ProgramState = {

    println("Statement view:")
    val chosenArguments = backtracking.filter {
      case (arg, _) => arguments.contains(arg)
    }

    val chosenArgumentsDefences = chosenArguments.values.flatten.toMap
    val defencesZipped = chosenArgumentsDefences.keys.zipWithIndex

    println(zippedToString(defencesZipped))

    generateABRepresentation(highlightedData = Some(StatementsWithinArgumentToChooseFrom(arguments, chosenArgumentsDefences.keySet, proponent = true)))

    getUserInput match {
      case "" =>
        moveView(chosenArgumentsDefences)(arguments, backtracking)
      case "b" => argumentView(backtracking)
      case "q" => programState // exit
      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val defMatching = defencesZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        val chosenDefencesMoves = chosenArgumentsDefences.filter {
          case (s, _) => defMatching.contains(s)
        }

        moveView(chosenDefencesMoves)(arguments, backtracking)

      case _ =>
        println("Invalid input.")
        statementsView(arguments)(backtracking)
    }
  }

  @tailrec
  def moveView(moves: Map[String, Seq[PotentialMove2]])
              (backtracking1: Set[ArgumentTree], backtracking2: Map[ArgumentTree, Set[(String, Seq[PotentialMove2])]])
              (implicit programState: ProgramState): ProgramState = {

    println("Rule/assumption view:")


    implicit val framework = programState.framework
    implicit val dState = programState.currentDState

    val movesToPerform = moves.values.flatten
    val movesZipped = movesToPerform.zipWithIndex

    generateABRepresentation(highlightedData = Some(StatementsWithinArgumentToChooseFrom(backtracking1, moves.keySet, proponent = true)))

    println(zippedToString(movesZipped))

    getUserInput match {
      case "" =>
        println("Choose move to perform")
        moveView(moves)(backtracking1, backtracking2)
      case "b" => statementsView(backtracking1)(backtracking2)
      case "q" => programState // exit
      case moveIndex if digitRegex.matches(moveIndex) =>
        val index = moveIndex.toInt
        val moveToPerformOpt = movesZipped.find { case (_, i) => i == index }
        moveToPerformOpt match {
          case Some((move, _)) =>

            programState.copy(
              performedMoves = programState.performedMoves :+ move,
              currentDState = move.perform,
              redraw = true
            )

          case _ =>
            println("Invalid move index.")
            moveView(moves)(backtracking1, backtracking2)
        }

      case _ =>
        println("Invalid input.")
        moveView(moves)(backtracking1, backtracking2)
    }
  }

  override def toString: String = "Opponent Attacking"
}
