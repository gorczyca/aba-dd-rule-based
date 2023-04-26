package aba.reasoner.automatic


import aba.fileParser.FileParser
import aba.framework.Framework
import aba.move.TerminationCriteria.{TA, TC, TS}
import aba.move.DisputeAdvancement.{DABF, DC, DS}
import aba.reasoner.DisputeState
import aba.reasoner.automatic.Semantics.{Admissible, Complete, Semantics, Stable}
import aba.reasoner.automatic2.DisputeStateAuto2
import aba.reasoner.automatic2.movesPreferenceBased.MovesPreferenceBasedAutomaticReasoner2
import org.scalatest.funsuite.AnyFunSuite

// TODO: the same structure as main? How to distinguish
import aba.reasoner.automatic.Semantics

import scala.io.Source
import scala.util.{Failure, Random, Success}


class AutomaticReasonerTest extends AnyFunSuite {

  val INSTANCE_GOAL_RESULTS_FILE_LOC = "./testResources/aspforaba_results.csv"

  val DEFENCES_ADMISSIBLE = "./testResources/aspforaba_defences_adm.csv"
  val DEFENCES_COMPLETE = "./testResources/aspforaba_defences_com.csv"
  val DEFENCES_STABLE = "./testResources/aspforaba_defences_stb.csv"

  val FRAMEWORKS_DIR = "./benchmarks/flexABle_instances"

  //testCorrectness(1.0, Semantics.Admissible)
  //testCorrectness(0.25, Semantics.Stable, Some(1200))
  //testCorrectness(1.0, Semantics.Stable, Some(1200))

  //testDefencesCorrectness(Semantics.Admissible, 1.0)
  //testDefencesCorrectness(Semantics.Admissible,  1.0, onlyOne = false)
  //testDefencesCorrectness(Semantics.Admissible, .05, onlyOne = false, Some(60))
  //testDefencesCorrectness(Semantics.Complete, 0.01, onlyOne=true, Some(30))
  //testDefencesCorrectness(Semantics.Complete, 0.05, Some(30))
  //testDefencesCorrectness(Semantics.Stable, 0.05, onlyOne = false, Some(120))
  //testDefencesCorrectness(Semantics.Complete, 0.75, onlyOne = false, Some(120))
  //testDefencesCorrectness(Semantics.Stable, 0.75, onlyOne = false, Some(120))


  private def getInstancePath(instance: String): String = {
    s"$FRAMEWORKS_DIR/$instance"
  }



  // Checking correctness (whether both system decide acceptability or not)
  private def testCorrectness(percentage: Double, semantics: Semantics, timeoutOpt: Option[Int] = None): Unit = {
    val triples = getInstanceGoalResultTriples(semantics)
    val selectedTriples = getRandomSublist(triples, percentage)
    val totalSize = selectedTriples.size


    test(s"$semantics: ${(percentage * 100).toInt}% instances (flexABle and aspforaba)") {

      selectedTriples.zipWithIndex.foreach {
        case ((instance, goal, result), index) =>

          println(s"${index + 1}/$totalSize")

          val flexABleResult = getflexABleResult(instance, goal, semantics, timeoutOpt)
          flexABleResult match {
            case Some(res) => assert(result == res, s"Info: $instance $goal $semantics")
            case _ => println(s"Timeout.")
          }
      }
    }
  }

  private def getSemanticsReasoner(semantics: Semantics): MovesPreferenceBasedAutomaticReasoner2 = {
    semantics match {
      case Admissible => AutomaticReasonerMock.reasoner
      case Stable => AutomaticReasonerMock.reasoner.copy(tCriteriaType = TS, dAdvancementType = DS)
      case Complete => AutomaticReasonerMock.reasoner.copy(tCriteriaType = TC, dAdvancementType = DC)
    }
  }

  private def getflexABleResult(instance: String, goal: String, semantics: Semantics, timeoutOpt: Option[Int]): Option[Boolean] = {

    val instancePath = getInstancePath(instance)
    val reasoner = getSemanticsReasoner(semantics)

    FileParser("apx", instancePath) match {
      case Failure(exception) => throw exception
      case Success(f) =>
        implicit val framework: Framework = f.copy(goals = Set(goal))

        framework.isEvenPossible match {
          case (Some(_), _) =>
            Some(false)
          case (_, Some(_)) =>
            Some(false)
          case _ =>

            // TODO
            val initialState = DisputeState.initial(framework)
            val (tc, ad) = reasoner.initialTCAndDA
            val initialStateAuto = DisputeStateAuto2(initialState, Set.empty, Set.empty, Nil, tc, ad)


            reasoner.getNewIncompleteSuccessfulDSAndStackRec(List(initialStateAuto), Nil)(framework, onlyOne = true, timeoutOpt) match {
              case (_, _, true, _) => None
              case (_, successfulList, _, _) => Some(successfulList.nonEmpty)
            }
        }
    }
  }

  private def getResultForSemantics(semantics: Semantics): Array[String] => String = {
    semantics match {
      case Admissible => x => x(2)
      case Complete => x => x(2)
      case Stable => x => x(3)
    }
  }

  private def getInstanceGoalResultTriples(semantics: Semantics): List[(String, String, Boolean)] = {
    val source = Source.fromFile(INSTANCE_GOAL_RESULTS_FILE_LOC)
    val triples = source.getLines().toList.drop(1)
      .map(_.filterNot(_.isWhitespace).split(","))
        .map(array => (array(0), array(1),  getResultForSemantics(semantics)(array) == "yes"))
    source.close()
    triples
  }

  private def getRandomSublist[A](instancesGoalsPairs: List[A], percentage: Double): List[A] = {
    val numberOfPairs = (instancesGoalsPairs.size * percentage).toInt
    //Random.shuffle(instancesGoalsPairs).take(numberOfPairs)
    instancesGoalsPairs.take(numberOfPairs)
  }

  // Checking the obtained defence set (if the same got obtained by aspforaba)

  private def getDefencesCsvFile(semantics: Semantics): String = {
    semantics match {
      case Admissible => DEFENCES_ADMISSIBLE
      case Stable => DEFENCES_STABLE
      case Complete => DEFENCES_COMPLETE
    }
  }

  private def getFlexABleDefenceSet(instance: String, goal: String, semantics: Semantics, onlyOne: Boolean, timeoutOpt: Option[Int]): Option[Set[Set[String]]] = {
    val instancePath = getInstancePath(instance)
    FileParser("apx", instancePath) match {
      case Failure(exception) => throw exception
      case Success(f) =>
        implicit val framework: Framework = f.copy(goals = Set(goal))

        framework.isEvenPossible match {
          case (Some(_), _) =>
            Some(Set.empty)
          case (_, Some(_)) =>
            Some(Set.empty)
          case _ =>

            val reasoner = getSemanticsReasoner(semantics)
            // TODO
            val initialState = DisputeState.initial(framework)
            val (tc, ad) = reasoner.initialTCAndDA
            val initialStateAuto = DisputeStateAuto2(initialState, Set.empty, Set.empty, Nil, tc, ad)


            reasoner.getNewIncompleteSuccessfulDSAndStackRec(List(initialStateAuto), Nil)(framework, onlyOne, timeoutOpt) match {
              case (_, _, true, _) => None
              case (_, successfulList, _, _) =>
                  Some(successfulList.map(_.dState.defences).toSet)
              }

        }
    }


  }

  private def getInstanceGoalDefencesSetsTriples(semantics: Semantics, percentage: Double): List[(String, String, Set[Set[String]])] = {
    val inputFile = getDefencesCsvFile(semantics)
    //val inputFile = "./testResources/mock.csv"
    val source = Source.fromFile(inputFile)
    val lines = source.getLines().toList.drop(1)

    val numberOfInstances = (lines.size * percentage).toInt

    val triples = Random.shuffle(lines).take(numberOfInstances)
      .map(_.split(",", -1))
      .map(array => {
        val defenceSets = if (array(2).isEmpty) Set.empty[Set[String]]
        else array(2).filterNot(Set('[',']')).split(";", -1).map {
          case "" => Set.empty[String]
          case stringSet => stringSet.split(' ').toSet
        }.toSet

        (array(0), array(1), defenceSets)
      })
    source.close()
    triples
  }

  private def testDefencesCorrectness(semantics: Semantics, percentage: Double, onlyOne: Boolean = true, timeoutOpt: Option[Int] = None): Unit = {

    val triples = getInstanceGoalDefencesSetsTriples(semantics, percentage)
    //val selectedTriples = getRandomSublist(triples, percentage)
    val totalSize = triples.size

    test(s"$semantics-defences: ${(percentage * 100).toInt}% instances (flexABle and aspforaba)") {

      triples.zipWithIndex.foreach {
        case ((instance, goal, defencesSet), index) =>

          //println(s"${index + 1}/$totalSize")

          val flexABleResult = getFlexABleDefenceSet(instance, goal, semantics, onlyOne, timeoutOpt)
          flexABleResult match {
            case Some(flexABleDefences) =>

              println(s"${index + 1}/$totalSize\tcount: ${flexABleDefences.size} - ${defencesSet.size}, ${(flexABleDefences.size == defencesSet.size)} ")

              //if (!res) assert(defencesSet.isEmpty, s"Info $instance $goal unsat.")
              //else assert(flexABleDefences.subsetOf(defencesSet), s"Info: $instance $goal defence sets contained.\n\nFlexABle:$flexABleDefences\n\n")
              assert(flexABleDefences.subsetOf(defencesSet), s"Info: $instance $goal defence sets contained.\n\nFlexABle:$flexABleDefences\n\n")

            case _ => println(s"Timeout.")
          }
      }
    }
  }
}
