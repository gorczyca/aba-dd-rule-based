package interface.dotConverters

import dot.RBDotConverter
import interface.ProgramState

object RuleDotRepresentationInterface {
  def generateRuleRepresentation(outputFileName: String = "temp_rule.dot")(implicit programState: ProgramState): Unit = {
    RBDotConverter.exportDotRepr(gradientFill = true, outputFileName)(programState.currentDState, programState.framework)
  }
}
