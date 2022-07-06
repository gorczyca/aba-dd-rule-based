package experiments.generator

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.{DAB, DABF}
import aba.move.{PB1Move, PF1Move}
import aba.reasoner.{DisputeState, PotentialRuleMove}
import experiments.runner.ExperimentsRunner.{createCSVString, exportToCSV}

import java.io.File
import scala.annotation.tailrec
import scala.util.Random

object ExperimentsGenerator {

  val INPUT_ABA_FRAMEWORKS = "C:\\Projects\\aba_experiments_new\\aba-experiments\\instances\\rule_dd_instances"
//  val INPUT_ABA_FRAMEWORKS = "./../aba-experiments/rule_dd_instances/"
//  val STATEMENTS_COUNT = 20
  val STATEMENTS_COUNT = 10
  val OUTPUT_CSV_GOALS_FILE = "./../aba_experiments/nonfiltered_goals.csv"
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

    val dirFiles = new File(INPUT_ABA_FRAMEWORKS).listFiles.filter(_.isFile).toList

    val filesCount = dirFiles.size
    val allInstanceGoalPairs = dirFiles.zipWithIndex.flatMap{ case (file, index) => {
      println(s"File ${index+1}/${filesCount}")


      implicit val framework: Framework = Framework("apx", file.getAbsolutePath)
      val statements = pickStatements(framework, STATEMENTS_COUNT)

      val statementsSize = statements.size

      //implicit val framework: Framework = Framework("apx", "C:\\Projects\\aba-experiments - Copy\\instances\\rule_dd_instances\\exp_acyclic_depvary_step5_batch_yyy04.pl")
      //val statements = Set("z1")

      if (statements.size != STATEMENTS_COUNT) println(s"Less than ${STATEMENTS_COUNT} for ${file.getName}")

      statements.toList.zipWithIndex.map { case (st, index2) => {

        println(s"Stmt ${index2 + 1}/${statementsSize}")

        Seq(file.getName,
          st,
          isGoalSelfContradicting(st),
          isGoalDerivable(st, constraints = framework.assumptions),
          isGoalDerivable(st, constraints = Set.empty),
          isGoalANonAttackedAssumption(st, framework)
        )
      }
      }
    }}


    val csvString = createCSVString(CSV_INDEX, allInstanceGoalPairs)
    exportToCSV(OUTPUT_CSV_GOALS_FILE, csvString)

    val nonTrivialPairs = allInstanceGoalPairs.filter {
      case Seq(_, _, false, false, true, false)  => true
      case _ => false
    }

    val nonTrivialCSVString = createCSVString(CSV_INDEX, nonTrivialPairs)
    exportToCSV(NON_TRIVIAL_OUTPUT_CSV_GOALS_FILE, nonTrivialCSVString)

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

  def isGoalDerivable(goal: String, constraints: Set[String])(implicit framework: Framework): Boolean = {

    framework.goals = Set(goal)
    framework.constraints = constraints

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

  def isGoalSelfContradicting(goal: String)(implicit framework: Framework): Boolean = {
    framework.goals = Set(goal)

    framework.isEvenPossible._1 match {
      case Some(_) => true
      case None => false
    }
  }
}
