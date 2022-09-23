package interface

import aba.framework.Framework
import aba.move.{DisputeAdvancement, TerminationCriteria}
import aba.move.DisputeAdvancement.{DAB, DC, DABF, DisputeAdvancementType}
import aba.move.Move.MoveType
import aba.move.TerminationCriteria.{TA, TC, TerminationCriteriaType}
import aba.reasoner.approximate.ApproximateReasoner
import aba.reasoner.automatic2.complex.{GroundedReasoner, PreferredReasoner}
import aba.reasoner.automatic2.movesPreferenceBased.MovesPreferenceBasedAutomaticReasoner2
import aba.reasoner.interactive.InteractiveReasoner
import aba.reasoner.{DisputeState, PotentialMove2}

case class ABDotConfig(showCircular: Boolean,
                       showIncomplete: Boolean,
                       showConflicted: Boolean)

object ProgramState {

//  private val defaultAdvancementType = DAB
  private val defaultAdvancementType = DABF
  private val defaultTerminationCriteria = TA

  def initial(implicit framework: Framework): ProgramState = {

    val initialDState = DisputeState.initial

//    val initialAutomaticReasoner = StatementBasedAutomaticReasoner2.default(defaultTerminationCriteria, defaultAdvancementType)
    val initialAutomaticReasoner = MovesPreferenceBasedAutomaticReasoner2.default(defaultTerminationCriteria, defaultAdvancementType)
    val initialApproximateReasoner = ApproximateReasoner.default(initialAutomaticReasoner)
    val initialInteractiveReasoner = InteractiveReasoner.default(defaultAdvancementType, defaultTerminationCriteria)

    val initialAutomaticCompleteReasoner = initialAutomaticReasoner.copy(tCriteriaType = TC, dAdvancementType = DC)
    val initialGroundedReasoner = new GroundedReasoner(initialAutomaticCompleteReasoner)
    val initialPreferredReasoner = new PreferredReasoner(initialAutomaticCompleteReasoner)

    val initialPossibleMoves = DisputeAdvancement(defaultAdvancementType).getPossibleMoves(framework, initialDState)
    val initialTerminationCriteriaOver = TerminationCriteria.checkIfOver(defaultAdvancementType, defaultTerminationCriteria)(framework, initialDState, initialPossibleMoves)

    ProgramState(
      currentDState = DisputeState.initial,
      framework = framework,
      possibleMoves = initialPossibleMoves,
      performedMoves = Nil,
      performedMovesChunks = Nil,
      dAdvancement = defaultAdvancementType,
      tCriteria = defaultTerminationCriteria,
      generateDot = false,
      generatedArg = true,
      redraw = false,
      showState = false,
      quit = false,
      interactiveOver = None,
      terminationCriteriaOver = initialTerminationCriteriaOver,
      automaticReasoner = initialAutomaticReasoner,
      approximateReasoner = initialApproximateReasoner,
      interactiveReasoner = initialInteractiveReasoner,
      preferredReasoner = initialPreferredReasoner,
      groundedReasoner = initialGroundedReasoner,
      abDotConfig = ABDotConfig(
        showCircular = true,
        showConflicted = true,
        showIncomplete = true
      )
    )
  }
}

case class ProgramState(currentDState: DisputeState,
                        framework: Framework,
                        possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                        performedMoves: List[PotentialMove2],
                        performedMovesChunks: List[List[PotentialMove2]],
                        dAdvancement: DisputeAdvancementType,
                        tCriteria: TerminationCriteriaType,
                        generateDot: Boolean,
                        generatedArg: Boolean,
                        redraw: Boolean,
                        showState: Boolean,
                        quit: Boolean,
                        interactiveOver: Option[Boolean],
                        terminationCriteriaOver: Option[Boolean],
                        automaticReasoner: MovesPreferenceBasedAutomaticReasoner2, // TODO:
                        approximateReasoner: ApproximateReasoner,
                        interactiveReasoner: InteractiveReasoner,
                        preferredReasoner: PreferredReasoner,
                        groundedReasoner: GroundedReasoner,
                        abDotConfig: ABDotConfig
                       ) {
  def resetDefaults(): ProgramState = {
    this.copy(redraw = false)
  }
}
