package interface.dotConverters

import dot.RBDotConverter
import interface.ProgramState

object RuleDotRepresentationInterface {
  def generateRuleRepresentation(outputFileName: String = "temp_rule.dot", showBlocked: Boolean = false)(implicit programState: ProgramState): Unit = {
    RBDotConverter.exportDotRepr(gradientFill = true, outputFileName, showBlocked)(programState.currentDState, programState.framework)
  }
}
