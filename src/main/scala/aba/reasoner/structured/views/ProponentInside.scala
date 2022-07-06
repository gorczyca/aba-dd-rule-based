package aba.reasoner.structured.views

import aba.framework.Framework
import aba.move.Move.PB1
import aba.move.PB1Move
import aba.reasoner.DisputeState
import aba.reasoner.argumentBased2.{ArgumentTree, DisputeStateAB2}
import aba.reasoner.structured.StructuredReasoner
import aba.reasoner.structured.StructuredReasoner.{digitRegex, indicesRegex, zippedToString}
import interface.ProgramState

import scala.annotation.tailrec
import scala.util.matching.Regex

object ProponentInside extends View {



  @tailrec
  override def goalView(implicit state: ProgramState): ProgramState = {

    val framework = state.framework
    val dState = state.currentDState

    val pendingGoals = framework.goals diff dState.pPlayedCompleteStatements
    val pendingCulpritCandidates = (framework.contrariesOf(dState.culprits) intersect dState.pStatements) diff dState.pPlayedCompleteStatements

    val pendingGoalsSize = pendingGoals.size


    val goalsZipped = pendingGoals.toList.zipWithIndex
    val culpritCandidatesZipped = pendingCulpritCandidates.toList.zip(LazyList.from(pendingGoalsSize))

    println("Goal view:")
    println("\tGOALS:")
    println(zippedToString(goalsZipped))

    println("\tCULPRIT CONTRARIES:")
    println(zippedToString(culpritCandidatesZipped))

    // TODO: all of that should be in some interface
    val read = Console.in.readLine
    read match {
      case "b" => StructuredReasoner.run
      case "" =>
        val allStatements = pendingGoals union pendingCulpritCandidates
        argumentView(allStatements)
      case "q" => state

      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val goalsMatching = goalsZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        val culpritCandidatesMatching = culpritCandidatesZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        argumentView(goalsMatching union culpritCandidatesMatching)

      case _ =>
        println("Invalid input.")
        goalView
    }
  }


  @tailrec
  def argumentView(goals: Set[String])(implicit state: ProgramState): ProgramState = {
    val dState = state.currentDState
    implicit val framework: Framework = state.framework

    val args = DisputeStateAB2.create_arguments(goals, dState.pRules)
    val argsZipped = args.toList.zipWithIndex

    println("Argument view:")
    println(zippedToString(argsZipped))

    val read = Console.in.readLine
    read match {
      case "" =>
        statementsView(args)(goals)
      case "b" => goalView
      case "q" => state // exit
      case argsIndices if indicesRegex.matches(argsIndices) =>
        val indicesSet = argsIndices.split(",").map(_.toInt).toSet
        val argsMatching = argsZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        statementsView(argsMatching)(goals)

      case _ =>
        println("Invalid input.")
        argumentView(goals)
    }
  }

  @tailrec
  def statementsView(arguments: Set[ArgumentTree])(backtracking: Set[String])(implicit programState: ProgramState): ProgramState = {

    println("Statement view:")


    val dState = programState.currentDState
    val statementsToFurtherExpand = (arguments.flatMap(_.endpoints.flatMap(endPoint => endPoint._2.flatMap(_.body) + endPoint._1.statement)) intersect dState.pPlayedUnexpandedStatements) diff dState.defences

    val statementsZipped = statementsToFurtherExpand.toList.zipWithIndex
    println(zippedToString(statementsZipped))

    val read = Console.in.readLine
    read match {
      case "" =>
        moveView(statementsToFurtherExpand)(arguments, backtracking)
      case "b" => argumentView(backtracking)
      case "q" => programState // exit
      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val stmtsMatching = statementsZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        moveView(stmtsMatching)(arguments, backtracking)

      case _ =>
        println("Invalid input.")
        statementsView(arguments)(backtracking)
    }
  }

  @tailrec
  def moveView(statements: Set[String])(backtracking1: Set[ArgumentTree], backtracking2: Set[String])(implicit programState: ProgramState): ProgramState = {

    println("Rule/assumption view:")


    implicit val framework: Framework = programState.framework
    implicit val dState: DisputeState = programState.currentDState

    val relevantMoves = programState.possibleMoves(PB1).filter {
      case PB1Move(rule, _) => statements.contains(rule.head)
    }

    val relevantMovesZipped = relevantMoves.zipWithIndex
    println(zippedToString(relevantMovesZipped))

    val read = Console.in.readLine
    read match {
      case "" =>
        println("Choose move to perform")
        moveView(statements)(backtracking1, backtracking2)
      case "b" => statementsView(backtracking1)(backtracking2)
      case "q" => programState // exit
      case moveIndex if digitRegex.matches(moveIndex) =>
        val index = moveIndex.toInt
        val moveToPerformOpt = relevantMovesZipped.find { case (_, i) => i == index }
        moveToPerformOpt match {
          case Some((move, _)) =>

            programState.copy(
              performedMoves = programState.performedMoves :+ move,
              currentDState = move.perform,
              redraw = true
            )

          case _ =>
            println("Invalid move index.")
            moveView(statements)(backtracking1, backtracking2)
        }

      case _ =>
        println("Invalid input.")
        moveView(statements)(backtracking1, backtracking2)
    }
  }

  override def toString: String = "Proponent Inside"
}
