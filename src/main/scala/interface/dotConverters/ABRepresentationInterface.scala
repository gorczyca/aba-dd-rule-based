package interface.dotConverters

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialAssumptionMove, PotentialMove2}
import aba.reasoner.argumentBased2.{ArgumentTree, DisputeStateAB2}
import dot.ABDotConverter
import interface.ProgramState

object ABRepresentationInterface {

  private def indicateOver(implicit programState: ProgramState): Option[Boolean]
    = (programState.interactiveOver, programState.terminationCriteriaOver) match {
      case (a@Some(_), _) => a
      case (_, b@Some(_)) => b
      case _ => None
  }

  def generateABRepresentation(outputFileName: String = "temp_arg.dot", additionalInformation: String = "")(implicit programState: ProgramState): String = {

    // TODO: clean
    val framework = programState.framework
    val dState = programState.currentDState
    val performedMoves = programState.performedMoves
    val dotConfig = programState.abDotConfig

    // assumptions used with PF2 that are not contained in any rule
    val propAssumptionGoals = performedMoves.filter(_.moveType.isProponentMove).collect {
      case asmMove: PotentialAssumptionMove if !dState.pRules.flatMap(_.statements).contains(asmMove.assumption) => asmMove.assumption
    }.toSet

    val actualPropGoals = framework.goals  union framework.contrariesOf(dState.culprits) union propAssumptionGoals

    def filterCircular: ArgumentTree => Boolean = arg => dotConfig.showCircular || !arg.isCircular
    def filterIncomplete: ArgumentTree => Boolean = arg => dotConfig.showIncomplete || arg.isComplete
    def filterConflicted: ArgumentTree => Boolean = arg => dotConfig.showConflicted || !arg.isConflicted

    val propArgs = DisputeStateAB2.create_arguments(actualPropGoals, dState.pRules)(framework)
      .filter(filterCircular)
      .filter(filterConflicted)
      .filter(filterIncomplete)

    // maximality
    val propArgs2 = propArgs.filter(arg => !(propArgs - arg).exists(otherArg => arg.rulesUsed.subsetOf(otherArg.rulesUsed)))

    // assumptions used in OF2 moves that are not parts of any rules used
    val oppAssumptionGoals = performedMoves.filter(_.moveType.isOpponentsMove).collect {
      case asmMove: PotentialAssumptionMove if !dState.bRules.flatMap(_.statements).contains(asmMove.assumption) => asmMove.assumption
    }.toSet

    val opponentGoals = (dState.defenceContraries intersect dState.bStatements) union oppAssumptionGoals // those that actually have been uttered
    val oppArgs = DisputeStateAB2.create_arguments(opponentGoals, dState.bRules)(framework)
      .filter(filterCircular)
      .filter(filterConflicted)
      .filter(filterIncomplete)

    // maximality
    val oppArgs2 = oppArgs.filter(arg => !(oppArgs - arg).exists(otherArg => arg.rulesUsed.subsetOf(otherArg.rulesUsed)))

    ABDotConverter.exportDotRepr(propArgs2, oppArgs2, outputFileName, additionalInformation, indicateOver)(dState, framework)
  }

}
