package experiments.generator

import aba.fileParser.FileParser
import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.DAB
import aba.move.PB1Move
import aba.reasoner.{DisputeState, PotentialRuleMove}
import experiments.runner.ExperimentsRunner.{createCSVString, exportToCSV}

import java.io.File
import scala.annotation.tailrec
import scala.util.{Failure, Random, Success}

object ExperimentsNonFilteringGenerator {

  val INPUT_ABA_FRAMEWORKS = "C:\\Projects\\aba_experiments_new\\aba-experiments\\instances\\rule_dd_instances"
  val STATEMENTS_COUNT = 10
  val OUTPUT_CSV_GOALS_FILE = "./../aba_experiments/unfiltered_goals.csv"

  // output csv columns
  val GOAL_COLUMN = "goal"
  val INSTANCE_COLUMN = "instance"

  val CSV_INDEX = Seq(INSTANCE_COLUMN, GOAL_COLUMN)

  def main(args: Array[String]): Unit = {

    val dirFiles = new File(INPUT_ABA_FRAMEWORKS).listFiles.filter(_.isFile).toList
    val allInstanceGoalPairs = dirFiles.flatMap(file => {

      FileParser("apx", file.getAbsolutePath) match {
        case Success(framework) =>
          val statements = pickStatements(framework, STATEMENTS_COUNT)
          statements.map(st => Seq(file.getName, st))
        case Failure(exception) => throw exception
      }
    })

    val csvString = createCSVString(CSV_INDEX, allInstanceGoalPairs)
    exportToCSV(OUTPUT_CSV_GOALS_FILE, csvString)

  }

  private def pickStatements(framework: Framework, statementsCount: Int): Set[String] = {

    if (framework.alphabet.size <= statementsCount) {
      framework.alphabet
    } else {
      val shuffledAlphabet = Random.shuffle(framework.alphabet.toList)
      shuffledAlphabet.take(statementsCount).toSet
    }
  }


}
