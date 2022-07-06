package experiments.runner

import aba.framework.Framework

import java.io.{File, FileWriter}
import aba.move.DisputeAdvancement.{DAB, DABF, DC, DS, DisputeAdvancementType}
import aba.move.TerminationCriteria.{TA, TC, TS, TerminationCriteriaType}
import aba.reasoner.{DisputeState, PotentialMove2}
import aba.reasoner.TOREMOVEautomatic.DisputeStateAuto
import aba.reasoner.automatic2.DisputeStateAuto2
import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2
import dot.DotConverter
import experiments.runner.finalExperiments.ExperimentsRunner2.{OUTPUT_DIR, convertDuration, createCSVString, createDirIfNotExists, exportToFile, getRuleDDOutput, outputDStateInformation}
import experiments.runner.finalExperiments.{ExperimentalParserConfig, ExperimentalRunnerParser}
import interface.{ABDotConfig, ProgramState}
import interface.dotConverters.ABRepresentationInterface.generateABRepresentation


@deprecated
object ComplexExperimentsRunner {

//  val INPUT_PATH = "C:\\Dresden\\master thesis\\31.03\\instances\\glaucoma-example-for-experiments\\glaucoma-example04-with-transitivity-02-all"
//  val INPUT_PATH = "C:\\Dresden\\master thesis\\31.03\\instances\\glaucoma-example-for-experiments\\glaucoma-example04-without-transitivity-all"
//  val INPUT_PATH = "C:\\Dresden\\master thesis\\31.03\\instances\\glaucoma-example-easier"
  val INPUT_PATH = "C:\\Dresden\\master thesis\\31.03\\instances"
//  val INPUT_PATH = "C:\\Dresden\\master thesis\\31.03\\instances\\should_work"

  val OUTPUT_DIR = "./output-glaucoma/"

  // Csv columns
  val INSTANCE_COLUMN = "instance"
  val GOAL_COLUMN = "goal"
  val VERDICT_COLUMN = "verdict"
  val DURATION_COLUMN = "duration"


  val CSV_INDEX = Seq(INSTANCE_COLUMN, GOAL_COLUMN, VERDICT_COLUMN, DURATION_COLUMN)


  def main(args: Array[String]): Unit = {

    ExperimentalRunnerParser.parse(args) match {
      case Some(config) => performExperiments(config)
      case _ => println("Error while parsing!")
    }
  }


  def performExperiments(config: ExperimentalParserConfig): Unit = {

    createDirIfNotExists(OUTPUT_DIR)
    createDirIfNotExists(s"$OUTPUT_DIR\\plots")

    val automaticReasoner = StatementBasedAutomaticReasoner2(
      dfs = config.dfs,
      tCriteriaType = config.tCriteriaType,
      dAdvancementType = config.dAdvancementType,
      startWithAdmissible = config.startWithAdmissible,
      turnChoice = config.turnChoice,
      pStatementChoice = config.pStatementChoice,
      oStatementChoice = config.oStatementChoice,
      pRuleChoiceType = config.pRuleChoiceType,
      oRuleChoiceType = config.oRuleChoiceType
    )

    val outputFilenameBase = s"${config.dAdvancementType}_${config.tCriteriaType}_" +
      s"${config.dfs}_${config.startWithAdmissible}_${config.turnChoice}_${config.pStatementChoice}_" +
      s"${config.oStatementChoice}_${config.pRuleChoiceType}_${config.oRuleChoiceType}_" +
      s"${config.timeout}"

    createDirIfNotExists(s"$OUTPUT_DIR/$outputFilenameBase")

    val dirFile = new File(config.instancesDirectory)
    val files = recursiveListFiles(dirFile)
    val abaFiles = files.filter(_.getName.endsWith(".aba")).toList

    val totalSize = abaFiles.length

    val experimentOutput = abaFiles.zipWithIndex.map {
      case (file, index) =>

        println(s"${index+1}/$totalSize")

        val framework = Framework.apply(config.inputFormat, file.getAbsolutePath)
        val (verdict, duration) = getRuleDDOutput(automaticReasoner, framework, config.tCriteriaType, config.dAdvancementType, outputFilenameBase, file.getName, config.timeout.toInt)
        Seq(file.getName, framework.goals.mkString(";"), verdict, duration)
    }

    val csvString = createCSVString(CSV_INDEX, experimentOutput)
    exportToFile(s"$OUTPUT_DIR\\$outputFilenameBase.csv", csvString)

  }


  def getRuleDDOutput(automaticReasoner: StatementBasedAutomaticReasoner2, framework: Framework, tCriteria: TerminationCriteriaType, dAdvancement: DisputeAdvancementType, outputDirPath: String, instanceName: String, timeout: Int): (String, Int) = {


    val initialDState = DisputeState.initial(framework)
    val initialDStateAuto = new DisputeStateAuto2(initialDState, Set.empty, Set.empty, Nil, tCriteria, dAdvancement)
    val initialStack = List(initialDStateAuto)

    // TODO: automatic reasoner should have termination criteria and advancement type

    // TODO: changed to DABF
    try {


      automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(initialStack, Nil)(framework, onlyOne = true, timeoutOpt = Some(timeout)) match {

        case (_, _, true, duration) => ("timeout", convertDuration(duration))
        case (_, Nil, false, duration) => ("no", convertDuration(duration))
        case (_, successfulHead :: _, false, duration) => {


          val programState = ProgramState.initial(framework).copy(
            currentDState = successfulHead.dState,
            framework = framework,
            performedMoves = successfulHead.performedMoves,
            abDotConfig = ABDotConfig(showCircular = true, showIncomplete = true, showConflicted = true)
          )


          generateABRepresentation(s"$OUTPUT_DIR\\plots\\${outputDirPath}_${instanceName}_arg.dot")(programState)
          //DotConverter.exportDotRepr(outputFileName = s"$OUTPUT_DIR\\plots\\b.dot")(headDs.dState, framework)

          // output additional information
          outputDStateInformation(successfulHead, framework.goals.head, instanceName, outputDirPath)

          ("yes", convertDuration(duration))
          // additionally here save performed moves, defences, culprits, rules / statements used by all
        }
      }
    } catch  {
      case e: Exception => {
        print("Error!")
        (s"exception;${e.getMessage}", 0)
      }
      case e: Throwable => {
        print("Error!")
        (s"exception;${e.getMessage}", 0)
      }
      case _ => {
        print("SomethingElse!")
        print("something else caught")
        ("error", 0)
      }
    }
  }


  def outputDStateInformation(disputeStateAuto2: DisputeStateAuto2, goal: String, instanceName: String, directory: String): Unit = {


    val lines = "// Performed moves:" + "\n" +
      disputeStateAuto2.performedMoves.mkString("\n") + "\n" +
      s"// Moves count: ${disputeStateAuto2.performedMoves.size}" + "\n" +
      s"// Defences: ${disputeStateAuto2.dState.defences.mkString(";")}" + "\n" +
      s"// Culprits: ${disputeStateAuto2.dState.culprits.mkString(";")}" + "\n" +
      s"// Goal: ${goal}" + "\n" +
      s"// PStatements: ${disputeStateAuto2.dState.pStatements.mkString(";")}" + "\n" +
      s"// OStatements: ${disputeStateAuto2.dState.oStatements.mkString(";")}" + "\n" +
      s"// PRules: ${disputeStateAuto2.dState.pRules.mkString(";")}" + "\n" +
      s"// ORules: ${disputeStateAuto2.dState.pRules.mkString(";")}"

    val outputFileName = s"$OUTPUT_DIR/$directory/${instanceName}_${goal}_info.txt"
    exportToFile(outputFileName, lines)
  }


  def convertDuration(seconds: Double): Int = (seconds * 1e3).toInt


  private def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }


}
