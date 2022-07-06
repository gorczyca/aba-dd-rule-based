package aba.reasoner.structured.views

import aba.framework.Framework
import aba.move.Move.OB1
import aba.move.OB1Move
import aba.reasoner.DisputeState
import aba.reasoner.argumentBased2.{ArgumentTree, DisputeStateAB2}
import aba.reasoner.structured.StructuredReasoner
import interface.ProgramState
import aba.reasoner.structured.StructuredReasoner.{digitRegex, indicesRegex, zippedToString}

import scala.annotation.tailrec
import scala.util.matching.Regex

object OpponentInside extends View {

  @tailrec
  override def goalView(implicit state: ProgramState): ProgramState = {

    val framework = state.framework
    val dState = state.currentDState

    val bUnblockedStatements = dState.bStatements -- dState.bPlayedBlockedStatements
    val pendingDefencesContraries = (framework.contrariesOf(dState.defences) intersect bUnblockedStatements) -- dState.bUnblockedCompletePlayedStatements

    val contrariesZipped = pendingDefencesContraries.toList.zipWithIndex

    println("Goal view:")
    println("\tGOALS:")
    println(zippedToString(contrariesZipped))

    // TODO: all of that should be in some interface
    val read = Console.in.readLine
    read match {
      case "b" => StructuredReasoner.run
      case "" =>
        argumentView(pendingDefencesContraries)
      case "q" => state

      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val goalsMatching = contrariesZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        argumentView(goalsMatching)

      case _ =>
        println("Invalid input.")
        goalView
    }
  }


  @tailrec
  def argumentView(goals: Set[String])(implicit state: ProgramState): ProgramState = {
    val dState = state.currentDState
    implicit val framework: Framework = state.framework

    val bUnblockedRules = dState.bRules -- dState.bPlayedBlockedRules

    val args = DisputeStateAB2.create_arguments(goals, bUnblockedRules).filter(arg => !arg.isComplete && !arg.isCircular)
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

    val dState = programState.currentDState
    val framework = programState.framework
    val statementsToFurtherExpand = (arguments.flatMap(arg => arg.rulesUsed.flatMap(_.statements) + arg.root.statement) diff dState.bUnblockedCompletePlayedStatements) diff framework.assumptions

    println("Statement view:")
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

    implicit val framework: Framework = programState.framework
    implicit val dState: DisputeState = programState.currentDState

    println("Rule/assumption view:")


    val relevantMoves = programState.possibleMoves(OB1).filter {
      case OB1Move(rule, _) => statements.contains(rule.head)
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

  override def toString: String = "Opponent Inside"
}
