package experiments.runner.actualFinalExperiments

import aba.fileParser.FileParser
import aba.framework.Framework
import aba.reasoner.DisputeState
import aba.reasoner.automatic2.DisputeStateAuto2
import experiments.runner.actualFinalExperiments.ExperimentsMode.{AbaStrategy, Approximation, Grounded, Normal, Preferred}
import experiments.runner.finalExperiments.{ExperimentalParserConfig, ExperimentalRunnerParser}

import scala.util.{Failure, Success}

object Runner {

  // the following experiments are supported
  // credulous acceptance / enumeration for all advancement types / termination criteria: mode normal
  // approximate: mode approx
  // preferred / grounded: mode pref / grd

  def main(args: Array[String]): Unit = {
    ExperimentalRunnerParser.parse(args) match {
      case Some(config) => performExperiments(config)
      case _ => println("Parsing error!")
    }

  }

  def performExperiments(config: ExperimentalParserConfig): Unit = {

    FileParser(config.inputFormat, config.frameworkInputPath) match {
      case Failure(exception) => throw exception
      case Success(fram) =>

        implicit val framework: Framework  = config.goal match {
          case Some(goal) => fram.copy(goals = Set(goal))
          case _ => fram
        }

        val initialState = DisputeState.initial(framework)
        val (tc, ad) = config.getAutomaticReasoner.initialTCAndDA
        val initialStateAuto = DisputeStateAuto2(initialState, Set.empty, Set.empty, Nil, tc, ad)


        val successfulDSList = config.mode match {
          case Normal => normalDisputeDerivation(config, initialStateAuto)
          case AbaStrategy => normalDisputeDerivation(config, initialStateAuto, abaStrategy = true)
          case Approximation => approximateDisputeDerivation(config, initialStateAuto)
          case Grounded => groundedDisputeDerivation(config)
          case Preferred => preferredDisputeDerivation(config)
        }

        outputDDInformationList(successfulDSList)
    }
  }

  def outputDDInformation(successfulDD: DisputeStateAuto2)(implicit framework: Framework): String = {
    s"""
       |Goal: ${framework.goals.head}
       |Performed moves count: ${successfulDD.performedMoves.size}
       |${successfulDD.performedMoves.mkString("\n")}
       |Defences count: ${successfulDD.dState.defences.size}
       |Defences: [ ${successfulDD.dState.defences.mkString(";")} ]
       |Culprits count: ${successfulDD.dState.culprits.size}
       |Culprits: [ ${successfulDD.dState.culprits.mkString(";")} ]
       |Opponents assumptions count: ${ (successfulDD.dState.oStatements intersect framework.assumptions).size }
       |Opponents assumptions: [ ${ (successfulDD.dState.oStatements intersect framework.assumptions).mkString(";") } ]
       |Proponent rules count: ${successfulDD.dState.pRules.size}
       |Proponent rules: [ ${successfulDD.dState.pRules.mkString(";")} ]
       |Opponent rules count: ${successfulDD.dState.oRules.size}
       |Opponent rules: [ ${successfulDD.dState.oRules.mkString(";")} ]
       |Proponent statements count: ${successfulDD.dState.pStatements.size}
       |Proponent statements: [ ${successfulDD.dState.pStatements.mkString(";")} ]
       |Opponent statements count: ${successfulDD.dState.oStatements.size}
       |Opponent statements: [ ${successfulDD.dState.oStatements.mkString(";")} ]
       |Total statements count: ${successfulDD.dState.bStatements.size}
       |Total statements: [ ${successfulDD.dState.bStatements.mkString(";")} ]
       |Total rules count: ${successfulDD.dState.bRules.size}
       |""".stripMargin
  }

  def outputDDInformationList(successfulStatesList: List[DisputeStateAuto2])(implicit framework: Framework): Unit = {

    val size = successfulStatesList.size
    val distinctDefences = successfulStatesList.map(_.dState.defences).distinct
    val distinctDefencesSize = distinctDefences.size

    val distinctDefencesString = distinctDefences.sortBy(_.size).map(d => s"[ ${d.mkString(";")} ]").mkString("\n")
    // take only one maximally
    val stringToPrint = successfulStatesList.take(1).map(outputDDInformation).mkString("\n+++\n")
    println(s"Total successful derivations found: ${size}\n" +
            s"Total distinct defences count: ${distinctDefencesSize}\n" +
            s"Distinct defences:\n$distinctDefencesString\n" +
            s"\n+++\n" +
            s"$stringToPrint")

  }

  // normal dds
  // TODO: this should be in some interfaces, because its the same for each
  def normalDisputeDerivation(config: ExperimentalParserConfig,
                              initialState: DisputeStateAuto2,
                              abaStrategy: Boolean = false)
                             (implicit framework: Framework): List[DisputeStateAuto2] = {

    val automaticReasoner =
      if (abaStrategy) config.getStatementBasedReasoner
      else config.getAutomaticReasoner

    // there will be no timeout
    // no duration needed
    automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(List(initialState), Nil)(framework, config.onlyOne, None) match {
      case (_, successfulList, _, _) => successfulList
        // successfulList can either be
        //  - empty - if not acceptable
        //  - have 1 element -> if onlyOne = true
        //  - have multiple elements
    }
  }

  def approximateDisputeDerivation(config: ExperimentalParserConfig,
                                   initialState: DisputeStateAuto2)
                                  (implicit framework: Framework): List[DisputeStateAuto2] = {

    val approximateReasoner = config.getApproximateReasoner
    approximateReasoner.getNewIncompleteSuccessfulDSAndStackRec(List(initialState), Nil)(framework, config.onlyOne) match {
      case (_, successfulList, _, _) => successfulList
    }
  }

  def groundedDisputeDerivation(config: ExperimentalParserConfig)
                               (implicit framework: Framework): List[DisputeStateAuto2] = {


    // set framework goals to empty to find the first grounded, only then check if it contains goal
    val frameworkWithoutGoals = framework.copy(goals = Set.empty)
    val groundedReasoner = config.getGroundedReasoner
    groundedReasoner.findGroundedDisputeDerivations(0, List(Set.empty))(frameworkWithoutGoals, onlyOne = config.onlyOne) match {
      case (successfulList, _, _) => {
          successfulList.filter(s => framework.goals.subsetOf(s.dState.pPlayedCompleteStatements))
          //successfulList
        }
    }
  }

  def preferredDisputeDerivation(config: ExperimentalParserConfig)
                                (implicit framework: Framework): List[DisputeStateAuto2] = {

    val frameworkWithoutGoals = framework.copy(goals = Set.empty)
    val preferredReasoner = config.getPreferredReasoner
    preferredReasoner.findSuccessfulDDs(List(framework.assumptions), Nil, Set.empty, Set.empty, framework.goals, onlyOne = config.onlyOne)(frameworkWithoutGoals) match {
      case (successfulList, _, _) => successfulList.filter(s => framework.goals.subsetOf(s.dState.pPlayedCompleteStatements))
    }
  }
}
