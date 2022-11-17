package aba.reasoner.structured

import aba.reasoner.structured.views.ProponentAttacking.moveView
import aba.reasoner.structured.views.{OpponentAttacking, OpponentInside, ProponentAttacking, ProponentInside, View}
import interface.ProgramState
import interface.dotConverters.ABRepresentationInterface.generateABRepresentation
import interface.InputProcessorInterface.getUserInput

import scala.annotation.tailrec
import scala.util.matching.Regex


object StructuredReasoner {

  val indicesRegex: Regex = """^(\d+(,\d+)*)?$""".r // keep these regexes somewhere
  val digitRegex: Regex = """\d+""".r

  def zippedToString(statements: Iterable[(Any, Int)], addTab: Boolean = true): String = {
    statements.map {
      case (stmt, index) => s"${if (addTab) "\t" else ""}[$index] ${stmt.toString}"
    }.mkString("\n")
  }

  @tailrec
  def run(implicit state: ProgramState): ProgramState = {

    generateABRepresentation()


    val functions: Seq[View] = Seq(
      ProponentInside,
      ProponentAttacking,
      OpponentInside,
      OpponentAttacking
    )

    val functionsZipped = functions.zipWithIndex

    println(zippedToString(functionsZipped, addTab = false))

    getUserInput match {
      case "b" => state
      case moveIndex if digitRegex.matches(moveIndex) =>
        val index = moveIndex.toInt
        val chosenFunctionOpt = functionsZipped.find { case (_, i) => i == index }
        chosenFunctionOpt match {
          case Some((function, _)) =>
            function.goalView
          case _ =>
            println("Invalid index.")
            run
        }
      case _ =>
        println("Invalid input.")
        run
    }
  }


}
