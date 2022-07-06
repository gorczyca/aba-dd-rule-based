package aba.reasoner.structured.views

import aba.framework.{Contrary, Framework}
import aba.move.Move.{PB2, PF2}
import aba.move.{PB2Move, PF2Move}
import aba.reasoner.argumentBased2.{ArgumentTree, DisputeStateAB2}
import aba.reasoner.structured.StructuredReasoner
import aba.reasoner.{DisputeState, PotentialMove2}
import interface.ProgramState
import aba.reasoner.structured.StructuredReasoner.{digitRegex, indicesRegex, zippedToString}

import scala.annotation.tailrec
import scala.util.matching.Regex

object ProponentAttacking extends View {

  @tailrec
  def goalView(implicit state: ProgramState): ProgramState = {

    val framework = state.framework
    val dState = state.currentDState

    val playedUnblockedDefencesContraries = (state.currentDState.bStatements -- state.currentDState.bPlayedBlockedStatements) intersect dState.defenceContraries
    val contrariesZipped = playedUnblockedDefencesContraries.toList.zipWithIndex

    println("Goal view:")
    println("\tUNBLOCKED DEFENCES CONTRARIES:")
    println(zippedToString(contrariesZipped))

    // TODO: all of that should be in some interface
    val read = Console.in.readLine
    read match {
      case "b" => StructuredReasoner.run
      case "" =>
        argumentView(playedUnblockedDefencesContraries)
      case "q" => state

      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val contrariesMatching = contrariesZipped.filter {
          case (_, index) => indicesSet.contains(index)
        }.map(_._1).toSet

        argumentView(contrariesMatching)

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

    val args = DisputeStateAB2.create_arguments(goals, bUnblockedRules)
    val argsFiltered = args.filter(arg => !arg.isCircular)
    val argsZipped = argsFiltered.toList.zipWithIndex

    println("Argument view:")
    println(zippedToString(argsZipped))

    Console.in.readLine match {
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

    val framework = programState.framework
    val dState = programState.currentDState
    val culpritCandidates = arguments.flatMap(arg => arg.rulesUsed.flatMap(_.statements) + arg.root.statement) intersect framework.assumptions

    val candidatesZipped = culpritCandidates.toList.zipWithIndex
    println(zippedToString(candidatesZipped))

    val read = Console.in.readLine
    read match {
      case "" =>
        moveView(culpritCandidates)(arguments, backtracking)
      case "b" => argumentView(backtracking)
      case "q" => programState // exit
      case statementsIndices if indicesRegex.matches(statementsIndices) =>
        val indicesSet = statementsIndices.split(",").map(_.toInt).toSet
        val stmtsMatching = candidatesZipped.filter {
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

    val relevantMoves = programState.possibleMoves.filter {
      case (PB2 | PF2, _) => true
      case _ => false
    }


    val allMoves = relevantMoves.filter{
      case (PB2 | PF2, _) => true
      case _ => false
    }.values.flatten.filter {
      case PB2Move(rule, Some(_)) =>
        framework.contraries.filter { case Contrary(_, contrary) => contrary == rule.head }.map(_.assumption).intersect(statements).nonEmpty
        //attackedAssumptions.intersect(statements).nonEmpty
        // only those moves that the HEAD of the rule attacks, not all of them
      case PF2Move(_, Some(attackedAssumptions)) => attackedAssumptions.intersect(statements).nonEmpty
    }


    val allMovesZipped = allMoves.zipWithIndex

    println(zippedToString(allMovesZipped))

    val read = Console.in.readLine
    read match {
      case "" =>
        println("Choose move to perform")
        moveView(statements)(backtracking1, backtracking2)
      case "b" => statementsView(backtracking1)(backtracking2)
      case "q" => programState // exit
      case moveIndex if digitRegex.matches(moveIndex) =>
        val index = moveIndex.toInt
        val moveToPerformOpt = allMovesZipped.find { case (_, i) => i == index }
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

  override def toString: String = "Proponent Attacking"
}
