package aba.reasoner.automatic


import aba.fileParser.FileParser
import aba.framework.Framework
import aba.reasoner.DisputeState
import aba.reasoner.automatic2.DisputeStateAuto2
import org.scalatest.FunSuite

import scala.io.Source
import scala.util.{Failure, Random, Success}


class AutomaticReasonerTest extends FunSuite {

  val INSTANCE_GOAL_RESULT_TRIPLES_LOC = "./testResources/aspforaba_results.csv"
  val FRAMEWORKS_DIR = "./testResources/flexABle_instances"

  //testCorrectness(0.1)
  //testCorrectness(1.0)
  //testCorrectness(0.25)
  testCorrectness(0.5)


  def testCorrectness(percentage: Double): Unit = {
    val triples = getInstanceGoalResultTriples
    val selectedTriples = getRandomSublist(triples, percentage)
    val totalSize = selectedTriples.size


    test(s"Same results for aspforaba and flexABle for ${(percentage * 100).toInt}% of the instances") {

      selectedTriples.zipWithIndex.foreach {
        case ((instance, goal, result), index) =>

          println(s"${index + 1}/$totalSize")

          val flexABleResult = getflexABleResult(instance, goal)
          assert(result == flexABleResult)

      }
    }
  }

  private def getflexABleResult(instance: String, goal: String): Boolean = {

    val instancePath = s"$FRAMEWORKS_DIR/$instance"

    FileParser("apx", instancePath) match {
      case Failure(exception) => throw exception
      case Success(f) =>
        implicit val framework: Framework = f.copy(goals = Set(goal))

        framework.isEvenPossible match {
          case (Some(_), _) =>
            false
          case (_, Some(_)) =>
            false
          case _ =>

            // TODO
            val initialState = DisputeState.initial(framework)
            val (tc, ad) = AutomaticReasonerMock.reasoner.initialTCAndDA
            val initialStateAuto = DisputeStateAuto2(initialState, Set.empty, Set.empty, Nil, tc, ad)


            AutomaticReasonerMock.reasoner.getNewIncompleteSuccessfulDSAndStackRec(List(initialStateAuto), Nil)(framework, onlyOne = true, None) match {
              case (_, successfulList, _, _) => successfulList.nonEmpty
            }
        }
    }
  }

  private def getInstanceGoalResultTriples: List[(String, String, Boolean)] = {
    val source = Source.fromFile(INSTANCE_GOAL_RESULT_TRIPLES_LOC)
    val triples = source.getLines().toList.drop(1)
      .map(_.filterNot(_.isWhitespace).split(","))
        .map(pairArray => (pairArray(0), pairArray(1), pairArray(2) == "yes"))
    source.close()
    triples
  }

  private def getRandomSublist(instancesGoalsPairs: List[(String, String, Boolean)], percentage: Double): List[(String, String, Boolean)] = {
    val numberOfPairs = (instancesGoalsPairs.size * percentage).toInt
    Random.shuffle(instancesGoalsPairs).take(numberOfPairs)
  }
}
