package experiments.generator

import aba.fileParser.FileParser
import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.DABF
import aba.move.{PB1Move, PF1Move}
import aba.reasoner.{DisputeState, PotentialRuleMove}
import experiments.runner.ExperimentsRunner.{createCSVString, exportToCSV}

import java.io.File
import scala.annotation.tailrec
import scala.util.{Failure, Random, Success}
import scala.io.Source

object ExperimentsFilter {

  val INPUT_ABA_FRAMEWORKS = "C:\\Projects\\aba_experiments_new\\aba-experiments\\instances\\rule_dd_instances"
  val FRAMEWORK_GOAL_CSV_FILE = "C:\\Projects\\aba-experiments - Copy\\experiments\\rule_dd\\all_goals_cpy.csv"
//  val INPUT_ABA_FRAMEWORKS = "./../aba-experiments/rule_dd_instances/"
//  val STATEMENTS_COUNT = 20
  val STATEMENTS_COUNT = 10
  val OUTPUT_CLASSIFIED_CSV_GOALS_FILE = "goals_classified.csv"
  val OUTPUT_FILTERED_CSV_GOALS_FILE = "goals_filtered.csv"
  val NON_TRIVIAL_OUTPUT_CSV_GOALS_FILE = "./experiments/final_input.csv"

  // output csv columns
  val GOAL_COLUMN = "goal"
  val SELF_CONTRADICTING_COLUMN = "selfContradicting"
  val DERIVABLE_FROM_EMPTY_COLUMN = "derivableFromEmpty"
  val DERIVABLE_AT_ALL_COLUMN = "derivableAtAll"
  val NON_ATTACKED_ASSUMPTION_COLUMN = "nonAttackedAssumption"
  val INSTANCE_COLUMN = "instance"

  val CSV_INDEX = Seq(INSTANCE_COLUMN, GOAL_COLUMN, SELF_CONTRADICTING_COLUMN, DERIVABLE_FROM_EMPTY_COLUMN, DERIVABLE_AT_ALL_COLUMN, NON_ATTACKED_ASSUMPTION_COLUMN)


  def main(args: Array[String]): Unit = {

    val bufferedSource = Source.fromFile(FRAMEWORK_GOAL_CSV_FILE)
    //val test = bufferedSource.getLines.drop(1).zipWithIndex.toList
    val data = bufferedSource.getLines.drop(1).zipWithIndex.toList.map {
      case (line, index) =>
        println(index + 1)
        val frameworkName :: goal :: _ = line.split(",").toList
        val frameworkFullPath = s"$INPUT_ABA_FRAMEWORKS\\$frameworkName"

        FileParser("apx", frameworkFullPath) match {
          case Success(framework) =>


            Seq(
              frameworkName,
              goal,
              isGoalSelfContradicting(goal, framework),
              isGoalDerivable(goal, constraints = framework.assumptions, framework),
              isGoalDerivable(goal, constraints = Set.empty, framework),
              isGoalANonAttackedAssumption(goal, framework)
            )

          case Failure(exception) => throw exception
        }
    }

    val csvString = createCSVString(CSV_INDEX, data)
    exportToCSV(OUTPUT_CLASSIFIED_CSV_GOALS_FILE, csvString)

    val nonTrivialPairs = data.filter {
      case Seq(_, _, false, false, true, false)  => true
      case _ => false
    }

    val nonTrivialCSVString = createCSVString(CSV_INDEX, nonTrivialPairs)
    exportToCSV(OUTPUT_FILTERED_CSV_GOALS_FILE, nonTrivialCSVString)

    bufferedSource.close

//    val dirFiles = new File(INPUT_ABA_FRAMEWORKS).listFiles.filter(_.isFile).toList
//
//    val filesCount = dirFiles.size
//    val allInstanceGoalPairs = dirFiles.zipWithIndex.flatMap{ case (file, index) => {
//      println(s"File ${index+1}/${filesCount}")
//
//
//      implicit val framework: Framework = Framework("apx", file.getAbsolutePath)
//      val statements = pickStatements(framework, STATEMENTS_COUNT)
//
//      val statementsSize = statements.size
//
//      //implicit val framework: Framework = Framework("apx", "C:\\Projects\\aba-experiments - Copy\\instances\\rule_dd_instances\\exp_acyclic_depvary_step5_batch_yyy04.pl")
//      //val statements = Set("z1")
//
//      if (statements.size != STATEMENTS_COUNT) println(s"Less than ${STATEMENTS_COUNT} for ${file.getName}")
//
//      statements.toList.zipWithIndex.map { case (st, index2) => {
//
//        println(s"Stmt ${index2 + 1}/${statementsSize}")
//
//        Seq(file.getName,
//          st,
//          isGoalSelfContradicting(st),
//          isGoalDerivable(st, constraints = framework.assumptions),
//          isGoalDerivable(st, constraints = Set.empty),
//          isGoalANonAttackedAssumption(st, framework)
//        )
//      }
//      }
//    }}


//    val csvString = createCSVString(CSV_INDEX, allInstanceGoalPairs)
//    exportToCSV(OUTPUT_CSV_GOALS_FILE, csvString)
//
//    val nonTrivialPairs = allInstanceGoalPairs.filter {
//      case Seq(_, _, false, false, true, false)  => true
//      case _ => false
//    }
//
//    val nonTrivialCSVString = createCSVString(CSV_INDEX, nonTrivialPairs)
//    exportToCSV(NON_TRIVIAL_OUTPUT_CSV_GOALS_FILE, nonTrivialCSVString)

  }

  private def pickStatements(framework: Framework, statementsCount: Int): Set[String] = {

    if (framework.alphabet.size <= statementsCount) {
      framework.alphabet
    } else {
      val shuffledAlphabet = Random.shuffle(framework.alphabet.toList)
      shuffledAlphabet.take(statementsCount).toSet
    }
  }

  def isGoalANonAttackedAssumption(goal: String, framework: Framework): Boolean = {
    if (!framework.assumptions.contains(goal))  false
    else {
      // is an assumption
      val contraries = framework.contrariesOf(goal)
      // there are no rules attacking it                                                // attacker is not an assumption
      !framework.rules.exists { case Rule(_, head, _) => contraries.contains(head) } && (framework.assumptions intersect contraries).isEmpty
    }
  }

  def isGoalDerivable(goal: String, constraints: Set[String], fram: Framework): Boolean = {

    implicit val framework: Framework = fram.copy(
      goals = Set(goal),
      constraints = constraints
    )

    if (constraints.contains(goal))
      return false

    val initialState = DisputeState.initial
    val stack = initialState :: Nil

    @tailrec
    def checkIfDerivableRec(stack: List[DisputeState]): Boolean = {

      if (stack.isEmpty)
        false
      else {

        implicit val currentDState :: stackRest  = stack
        if (currentDState.pPlayedCompleteStatements.contains(goal))
          true
        else {

          // TODO: first try with PF1 moves
          val possiblePF1Moves = PF1Move.isPossible(DABF)
          if (possiblePF1Moves.nonEmpty) {
            val newState = possiblePF1Moves.head.perform
            val newStack = newState +: stackRest
            checkIfDerivableRec(newStack)
          } else {

            val possibleMoves = PB1Move.isPossible(DABF)
            if (possibleMoves.isEmpty)
              checkIfDerivableRec(stackRest)
            else {
              // group by head
              val grouped = possibleMoves.groupBy { case potM: PotentialRuleMove => potM.rule.head }

              val (_, moves) = grouped.head
              val newStack = moves.map(_.perform).toList
              checkIfDerivableRec(newStack ++ stackRest)
            }
          }
        }
      }
    }

    checkIfDerivableRec(stack)
  }

  def isGoalSelfContradicting(goal: String, fram: Framework): Boolean = {
    implicit val framework: Framework = fram.copy(goals = Set(goal))

    framework.isEvenPossible._1 match {
      case Some(_) => true
      case None => false
    }
  }
}
