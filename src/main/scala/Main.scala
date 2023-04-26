import aba.fileParser.FileParser

import scala.language.implicitConversions
import aba.framework.Framework
import aba.reasoner.DisputeState
import commandLineParser.CommandLineParser
import dot.DotConverter
import interface.ProgramState

import scala.util.{Failure, Success}
//import pureconfig.generic.auto._
//import pureconfig._
// only for a test

import interface.dotConverters.ABRepresentationInterface.generateABRepresentation
import interface.dotConverters.RuleDotRepresentationInterface.generateRuleRepresentation
import interface.InputProcessorInterface.processUserInput

import interface.reasoners.AutomaticReasonerInterface.findSuccessfulDerivations2

import java.io.PrintWriter
import scala.annotation.tailrec


object Main {

  def main(args: Array[String]): Unit = {

    // todo: this works
    //val c = ConfigSource.default.load[ServiceConf]

    CommandLineParser.parse(args) match {
      case Some(config) =>
        FileParser(config.inputFormat, config.inputFilePath) match {
          case Failure(exception) =>
            println(s"Error opening the input file.\n${exception.getMessage}")
          case Success(fram) =>

            implicit val framework: Framework = config.goal match {
              case Some(g) => fram.copy(goals = Set(g))
              case _ => fram
            }

            // TODO: take into account the config!!! in the constructor
            val initialState = ProgramState.initial

            val autoReasoner = initialState.automaticReasoner.copy(
              tCriteriaType = config.tCriteriaType,
              dAdvancementType = config.dAdvancementType,
              dfs = config.dfs,
              startWithAdmissible = config.startWithAdmissible,
              preferenceOrdering = config.preferenceOrdering,
              pRuleHeadChoice = config.pRuleHeadChoice,
              oRuleHeadChoice = config.oRuleHeadChoice,
              pRuleChoice = config.pRuleChoiceType,
              oRuleChoice = config.oRuleChoiceType)

            // if SOLVE mode simply try to solve and return
            if (config.solve) {
              findSuccessfulDerivations2(onlyOne = true, findAndReturn = true)(initialState)
              return
            }

            println("\n" +
              "+=======================+\n" +
              "|  Derivation started.  |\n" +
              "+=======================+\n")

            framework.isEvenPossible match {
              case (Some(contradictingGoals), _) =>
                println(s"""Goals ${contradictingGoals.mkString(". ")} are self-contradicting.""")
                return
              case (_, Some(constrainedGoals)) =>
                println(s"""Goals ${constrainedGoals.mkString(". ")} are constrained.""")
                return
              case _ =>
            }

            disputeDerivation(initialState
              .copy(tCriteria = config.tCriteriaType,
                dAdvancement = config.dAdvancementType,
                automaticReasoner = autoReasoner))

          case _ =>
        }
      case _ =>
    }
  }


  @tailrec
  def disputeDerivation(implicit programState: ProgramState): Unit = {

    // TODO: clean
    implicit val currentState: DisputeState = programState.currentDState   // get last derivation state
    implicit val framework: Framework = programState.framework

    if (programState.redraw) {
      val redrawInfo = programState.performedMoves.lastOption match {
        case Some(performedMove) =>
          val performedMoveInfo = s"" +
            s"+===================================+\n" +
            s"        Last performed move:\n" +
            s"${programState.performedMoves.size}: ${performedMove.toString}\n" +
            s"+===================================+\n"
          //s"\n${currentState.toString}"
          if (programState.showState) s"$performedMoveInfo:\n${currentState.toString}"
          else performedMoveInfo

        case _ => s"No moves performed."
      }
      println(s"$redrawInfo")
    }

    programState.terminationCriteriaOver match {
      case Some(propWon) => println(s"Game over. ${ if (propWon) "Proponent" else "Opponent" } won.")
      case _ =>
    }

    if (programState.generatedArg) generateABRepresentation()
    //if (programState.generateDot) DotConverter.exportDotRepr()
    if (programState.generateDot) generateRuleRepresentation()

    val newProgramState = processUserInput(programState)

    if (!newProgramState.quit) disputeDerivation(newProgramState)
    //else newDerivation
  }
}
