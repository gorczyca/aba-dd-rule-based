package aba.reasoner.automatic2

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.{DAB, DABF, DC, DS, DisputeAdvancementType}
import aba.move.TerminationCriteria.{TA, TC, TS, TerminationCriteriaType}
import aba.reasoner.{DisputeState, PotentialMove2}


object DisputeStateAuto2 {

  val hierarchy: Seq[(TerminationCriteriaType, DisputeAdvancementType)]  = Seq(
    //
    (TA, DABF),
    (TC, DC),
    (TS, DS)
  )

  def apply(potentialMove: PotentialMove2)(implicit currentDStateAuto: DisputeStateAuto2, framework: Framework): DisputeStateAuto2 = {
    implicit val dState: DisputeState = currentDStateAuto.dState
    currentDStateAuto.copy(
      dState = potentialMove.perform,
      performedMoves = currentDStateAuto.performedMoves :+ potentialMove
      //,
      //ignoredCurrentlyDefendedAssumptions = currentDStateAuto.ignoredCurrentlyDefendedAssumptions -- potentialMove.movePieces.newDefences
    )
  }

  def apply(potentialMove: PotentialMove2, newStatements: Set[String])(implicit currentDStateAuto: DisputeStateAuto2, framework: Framework): DisputeStateAuto2 = {
    this.apply(potentialMove).copy(proponentsStatementsToProveOpt = Some(newStatements))
  }


}


case class DisputeStateAuto2(dState: DisputeState,
                            ignoredProponentAssumptions: Set[String],
                            ignoredCulpritCandidates: Set[String],
                            //ignoredCurrentlyDefendedAssumptions: Set[String],
                            performedMoves: List[PotentialMove2],
                            // TODO: TEMPORARY
                            currentTerminationCriteria: TerminationCriteriaType,
                            currentAdvancementType: DisputeAdvancementType,
                            proponentsStatementsToProveOpt: Option[Set[String]] = None
                            ) {
  def performedMovesToString(initialVal: Int = 0): List[String] = {
    val numberLength = performedMoves.length.toString.length
    performedMoves.zipWithIndex.map { case (arg, index) => s"%0${numberLength}d".format(index + 1 + initialVal) + s": [${arg.toString}]" }
  }
}

