package experiments.runner

import aba.framework.Framework
import experiments.runner.ComplexExperimentsRunner.recursiveListFiles

import java.io.File

object CheckFrameworks {

  val frameworksPath = "C:\\Projects\\aba_experiments_new\\aba-experiments\\instances\\rule_dd_instances"
  val inputFileType = "apx"

  def main(args: Array[String]): Unit = {
    val dirFile = new File(frameworksPath)
    val files = recursiveListFiles(dirFile)
    val plFiles = files.filter(_.getName.endsWith(".pl")).toList

    val frameworks = plFiles.map(f => Framework(inputFileType, f.getAbsolutePath))

    val moreThanOneContrariesFrameworks = frameworks.filter(framework =>
      (framework.assumptions.size != framework.contraries.size) || (framework.contraries.toList.map(_.assumption).size != framework.assumptions.size)
        )

    val frameworksWithContrariesThatAreAssumptions = frameworks.filter(
      fram => (fram.assumptions intersect fram.contraries.map(_.contrary)).nonEmpty
    )

    val nonFlatFrameworks = frameworks.filter(
      fram => (fram.rules.map(_.head) intersect fram.assumptions).nonEmpty
    )

    val x = 1
  }

  private def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }


}
