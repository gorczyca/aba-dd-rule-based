package experiments.generator

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.DAB
import aba.move.PB1Move
import aba.reasoner.{DisputeState, PotentialRuleMove}
import experiments.runner.ExperimentsRunner.{createCSVString, exportToCSV}

import java.io.File
import scala.annotation.tailrec
import scala.util.Random

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

      implicit val framework: Framework = Framework("apx", file.getAbsolutePath)
      val statements = pickStatements(framework, STATEMENTS_COUNT)
      statements.map(st => Seq(file.getName, st))
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
