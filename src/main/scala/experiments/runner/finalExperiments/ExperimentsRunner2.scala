package experiments.runner.finalExperiments

import aba.framework.Framework
import aba.move.DisputeAdvancement.{DABF, DisputeAdvancementType}
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.DisputeState
import aba.reasoner.automatic2.DisputeStateAuto2
import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2
import java.io.File

import java.io.PrintWriter
import scala.io.Source

@deprecated
object ExperimentsRunner2 {

  val INSTANCES_DIR = "C:\\Projects\\aba_experiments_new\\aba-experiments\\instances\\rule_dd_instances"


  val OUTPUT_DIR = "./output/"

  // Csv columns
  val INSTANCE_COLUMN = "instance"
  val GOAL_COLUMN = "goal"
  val VERDICT_COLUMN = "verdict"
  val DURATION_COLUMN = "duration"


  val CSV_INDEX = Seq(INSTANCE_COLUMN, GOAL_COLUMN, VERDICT_COLUMN, DURATION_COLUMN)


  def performExperiments(config: ExperimentalParserConfig): Unit = {

    createDirIfNotExists(OUTPUT_DIR)

    val source = Source.fromFile(config.csvInputPath)
    val lines = source.getLines.toList.drop(1) // drop csv header
    source.close()

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

    // ensure created directory
    createDirIfNotExists(s"$OUTPUT_DIR/$outputFilenameBase")

    val totalInstancesCount = lines.size
    val experimentOutput = lines.zipWithIndex.map {
      case (line, index) =>

        println(s"${index + 1}/$totalInstancesCount")

        val fileName :: goal :: _ = line.split(",").toList
        val absoluteFilePath = s"${config.instancesDirectory}/$fileName"

        val framework = Framework(config.inputFormat, absoluteFilePath)
        framework.goals = Set(goal)

        val (verdict, duration) = getRuleDDOutput(automaticReasoner, framework, config.tCriteriaType, config.dAdvancementType, outputFilenameBase, fileName, config.timeout.toInt)
        Seq(fileName, goal, verdict, duration)
    }


    val csvString = createCSVString(CSV_INDEX, experimentOutput)
    exportToFile(s"$OUTPUT_DIR/$outputFilenameBase.csv", csvString)

  }


  def main(args: Array[String]): Unit = {

    ExperimentalRunnerParser.parse(args) match {
      case Some(config) => performExperiments(config)
      case _ => println("Parsing error!")
    }
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

  def createDirIfNotExists(dirpath: String): Unit = {
    val directory = new File(dirpath)
    if (!directory.exists) {
      directory.mkdir
      // If you require it to make the entire directory path including parents,
      // use directory.mkdirs(); here instead.
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
      s"// ORules: ${disputeStateAuto2.dState.oRules.mkString(";")}"

    val outputFileName = s"$OUTPUT_DIR/$directory/${instanceName}_${goal}_info.txt"
    exportToFile(outputFileName, lines)
  }


  def convertDuration(seconds: Double): Int = (seconds * 1e3).toInt

  def createCSVString(index: Seq[String], csvRows: List[Seq[Any]]): String =
    (index +: csvRows).map(_.mkString(",")).mkString("\n")

  def exportToFile(fileName: String, fileString: String): Unit = {
    new PrintWriter(fileName) {
      write(fileString) // TODO: implicit conversion of list of Seq to CSV
      close()
    }
  }
}
