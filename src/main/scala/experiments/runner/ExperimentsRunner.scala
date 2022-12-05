package experiments.runner

import aba.fileParser.FileParser
import aba.framework.Framework
import aba.move.DisputeAdvancement.{DAB, DABF}
import aba.move.TerminationCriteria.TA
import aba.reasoner.DisputeState
import aba.reasoner.TOREMOVEautomatic.{AutomaticReasoner, DisputeStateAuto}

import java.io.PrintWriter
import scala.io.Source
import scala.util.{Failure, Success}

object ExperimentsRunner {

  val INPUT_ABA_FRAMEWORKS = "./../aba-experiments/instances/rule_dd_instances/"
  val NON_TRIVIAL_CSV_GOALS_FILE = "./experiments/non_trivial.csv"
//  val NON_TRIVIAL_CSV_GOALS_FILE = "./experiments/non_trivial - Copy.csv"
  val EXPERIMENTS_OUTPUT_CSV = "./experiments/ruledd_results_new.csv"

  // Csv columns
  val INSTANCE_COLUMN = "instance"
  val GOAL_COLUMN = "goal"
  val VERDICT_COLUMN = "verdict"
  val DURATION_COLUMN = "duration"

  val TIMEOUT = 120

  val CSV_INDEX = Seq(INSTANCE_COLUMN, GOAL_COLUMN, VERDICT_COLUMN, DURATION_COLUMN)

  def main(args: Array[String]): Unit = {

    val source = Source.fromFile(NON_TRIVIAL_CSV_GOALS_FILE)
    val lines = source.getLines().toList.drop(1)
    source.close()

    val totalInstancesCount = lines.size
    val experimentOutput = lines.zipWithIndex.map {
      case (line, index) =>

        println(s"${index + 1}/$totalInstancesCount")

        val fileName :: goal :: _ = line.split(",").toList
        val absoluteFilePath = s"$INPUT_ABA_FRAMEWORKS/$fileName"

        val (verdict, duration) = getRuleDDOutput(AutomaticReasoner(), absoluteFilePath, goal)
        Seq(fileName, goal, verdict, duration)
    }

    val csvString = createCSVString(CSV_INDEX, experimentOutput)
    exportToCSV(EXPERIMENTS_OUTPUT_CSV, csvString)
  }

  def getRuleDDOutput(automaticReasoner: AutomaticReasoner, instancePath: String, goal: String): (String, Int) = {


    FileParser("apx", instancePath) match {
      case Failure(exception) => throw exception
      case Success(fram) =>
        implicit val framework: Framework = fram.copy(goals = Set(goal))


        val initialDStateAuto = DisputeStateAuto(DisputeState.initial, Set.empty, Set.empty, Nil)
        val initialStack = List(initialDStateAuto)

        // TODO: automatic reasoner should have termination criteria and advancement type

        // TODO: changed to DABF
        automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(initialStack, Nil)(framework, onlyOne = true, Some(TIMEOUT), TA, DABF) match {
          case (_, _, true, duration) => ("timeout", convertDuration(duration))
          case (_, Nil, false, duration) => ("no", convertDuration(duration))
          case (_, _ :: _, false, duration) => ("yes", convertDuration(duration))
        }
    }
  }


  def convertDuration(seconds: Double): Int = (seconds * 1e3).toInt

  def createCSVString(index: Seq[String], csvRows: List[Seq[Any]]): String =
    (index +: csvRows).map(_.mkString(",")).mkString("\n")

  def exportToCSV(csvName: String, csvString: String): Unit = {
    new PrintWriter(csvName) {
      write(csvString) // TODO: implicit conversion of list of Seq to CSV
      close()
    }
  }
}

