package aba.move

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialMove2}
import aba.move.DisputeAdvancement.{DAB, DC, DF, DS, DABF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.{TC, TS}

object TerminationCriteria extends Enumeration {
  type TerminationCriteriaType = Value

  val
   TA,
   TC,
   TS
  = Value

  implicit def fromString(terminationTypeString: String): TerminationCriteriaType = values.find(_.toString.equalsIgnoreCase(terminationTypeString)) match {
    case Some(value) => value
    case None => throw new Exception(s"No termination criteria type: $terminationTypeString")
  }

  // TODO change to val
  def proponentSeemsToBeWinning(implicit framework: Framework, dState: DisputeState, tCriteria: TerminationCriteriaType): Boolean = {
    val culpritContraries = framework.contrariesOf(dState.culprits) // TODO: keep this in dState

    // TODO lazy
    val condition1 = (dState.defenceContraries intersect dState.bUnblockedCompletePlayedStatements).isEmpty
    val condition2 = (framework.goals union culpritContraries).subsetOf(dState.pPlayedCompleteStatements)

    val additionalCondition = tCriteria match {
                                                                                        // TODO: do i need that?
                                                                                     // todo: better check that, because these can be ignored
                                                                                      // TODO: but here do not check moves possible but rather if
                                                                                      //  all statements derivable are contained
      //case TC => (dState.currentlyDefendedAssumptions -- dState.defences).isEmpty && Move(PF1).isPossible(DC).isEmpty
      case TC => (dState.currentlyDefendedAssumptions -- dState.defences).isEmpty && (dState.pStatementsFollowingFromPCompleteStatements -- dState.pStatements).isEmpty
      case TS => framework.assumptions == (dState.defences ++ dState.culprits)
      case _ => true
    }

    condition1 && condition2 && additionalCondition
  }

  private val terminationCriteriaAdvancementTypeMap = Map(
    TA -> Seq(DAB, DABF),
    TC -> Seq(DC),
    TS -> Seq(DS)
  )

  private def getTerminationCriteriaPossibleMoves(dAdvancementType: DisputeAdvancementType,
                                                    terminationCriteria: TerminationCriteriaType)
                                                   (implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                                                    framework: Framework,
                                                    dState: DisputeState): Map[MoveType, Seq[PotentialMove2]] = {

    if (terminationCriteriaAdvancementTypeMap(terminationCriteria).contains(dAdvancementType)) possibleMoves
    else {
      val tCriteriaAdvancement = terminationCriteriaAdvancementTypeMap(terminationCriteria).head
      DisputeAdvancement(tCriteriaAdvancement).getPossibleMoves
    }

  }

  private def checkIfOverTA(possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                            dFMoves: Map[MoveType, Seq[PotentialMove2]],
                            propSeemsWin: Boolean): Option[Boolean] = {

    // TODO: lazy

    // proponent cannot move
    // prop possible moves
    val possiblePropMoveTypes = Seq(PB1, PB2, PF2)  // filter.(...).isEmpty = !(...).exists
    val possibleMovePropCantMove = !possibleMoves.exists { case (mType, _) => possiblePropMoveTypes.contains(mType) }
    // prop df moves
    val dfPropMoveTypes = Seq(PF1, PF2)
    val dfMovesPropCantMove = !dFMoves.exists { case (mType, _) => dfPropMoveTypes.contains(mType) }

    // opponent cannot move
    // opp possible moves
    val possibleOppMoveTypes = Seq(OB1, OB2, OF2)
    val possibleMovesOppCantMove = !possibleMoves.exists { case (mType, _) => possibleOppMoveTypes.contains(mType) }
    // opp df moves
    val dfOppMoveTypes = Seq(OF1, OF2)
    val dfMovesOppCantMove = !dFMoves.exists { case (mType, _) => dfOppMoveTypes.contains(mType) }

    val isTAOver = true

    if (isTAOver && propSeemsWin && (possibleMovesOppCantMove || dfMovesOppCantMove)) Some(true)
    else if (isTAOver && !propSeemsWin && (possibleMovePropCantMove || dfMovesPropCantMove)) Some(false)
    else None
  }


  private def checkIfOverTC(possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                            dFMoves: Map[MoveType, Seq[PotentialMove2]],
                            propSeemsWin: Boolean): Option[Boolean] = {

    // TODO: lazy

    // proponent cannot move
    // prop possible moves
    val possibleMovePropCantMove = !possibleMoves.exists { case (mType, _) => mType.isProponentMove }
    // prop df moves
    val dfPropMoveTypes = Seq(PF1, PF2)
    val dfMovesPropCantMove = !dFMoves.exists { case (mType, _) => dfPropMoveTypes.contains(mType) }

    // opponent cannot move
    // opp possible moves
    val possibleOppMoveTypes = Seq(OB1, OB2, OF2)
    val possibleMovesOppCantMove = !possibleMoves.exists { case (mType, _) => possibleOppMoveTypes.contains(mType) }
    // opp df moves
    val dfOppMoveTypes = Seq(OF1, OF2)
    val dfMovesOppCantMove = !dFMoves.exists { case (mType, _) => dfOppMoveTypes.contains(mType) }

    // additional TC condition
    //val isTCOver = possibleMoves.isEmpty
    //val isTCOver = true

    // TODO: if this is necessary then possibleMoves(..) is not anymore
    //val isTCOver = !possibleMoves.exists { case (mType, _) => Seq(PF1, OF1).contains(mType) }

    //val cannotMovePf1 = !possibleMoves.exists { case (mType, _) => mType == PF1 }

    // additional move for the opponent,
    val opponentCannotMove = !possibleMoves.exists { case (mType, _) => mType.isOpponentsMove }
    //val opponentCannotMove = true


    //val fullyExpandedIncomplete = disputeState.bFullyExpandedStatements -- disputeState.bUnblockedCompletePlayedStatements
    //val propDoesntDefend = (disputeState.currentlyDefendedAssumptions -- disputeState.defences).exists(a => framework.contrariesOf(a).intersect(fullyExpandedIncomplete).nonEmpty)

    if (propSeemsWin && (possibleMovesOppCantMove || dfMovesOppCantMove)) Some(true)
    else if (!propSeemsWin && opponentCannotMove && (possibleMovePropCantMove || dfMovesPropCantMove)) Some(false)
    else None
  }


  private def checkIfOverTS(possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                            dFMoves: Map[MoveType, Seq[PotentialMove2]],
                            propSeemsWin: Boolean): Option[Boolean] = {

    // TODO: lazy

    // proponent cannot move
    // prop possible moves
    val possibleMovePropCantMove = !possibleMoves.exists { case (mType, _) => mType.isProponentMove }
    // prop df moves

    // opponent cannot move
    // opp possible moves
    val possibleOppMoveTypes = Seq(OB1, OB2, OF2)
    val possibleMovesOppCantMove = !possibleMoves.exists { case (mType, _) => possibleOppMoveTypes.contains(mType) }
    // opp df moves
    val dfOppMoveTypes = Seq(OF1, OF2)
    val dfMovesOppCantMove = !dFMoves.exists { case (mType, _) => dfOppMoveTypes.contains(mType) }

    if (propSeemsWin && (possibleMovesOppCantMove || dfMovesOppCantMove)) Some(true)
    else if (!propSeemsWin && possibleMovePropCantMove) Some(false)
    else None
  }


  def checkIfOver(dAdvancementType: DisputeAdvancementType,
                  terminationCriteriaType: TerminationCriteriaType,
                  filterMoves: PotentialMove2 => Boolean = _ => true)
                 (implicit framework: Framework,
                  dState: DisputeState,
                  currentlyPossibleMoves: Map[MoveType, Seq[PotentialMove2]]): Option[Boolean] = {

    // TODO: reuse currentlyPossibleMoves?

    val possibleMoves = getTerminationCriteriaPossibleMoves(dAdvancementType, terminationCriteriaType)
      .map { case(mType, moves) => (mType, moves.filter(filterMoves)) }.filter { case (_, moves) => moves.nonEmpty }

    // TODO: also filter here
    val dFMoves = DisputeAdvancement(DF).getPossibleMoves // todo: do I also have to filter here? TODO: think about it
      .map { case(mType, moves) => (mType, moves.filter(filterMoves)) }.filter { case (_, moves) => moves.nonEmpty }

    val propSeemsWin = proponentSeemsToBeWinning(framework, dState, terminationCriteriaType)

    terminationCriteriaType match {
      case TA => checkIfOverTA(possibleMoves, dFMoves, propSeemsWin)
      case TC => checkIfOverTC(possibleMoves, dFMoves, propSeemsWin) // TODO: do it smoother
      case TS => checkIfOverTS(possibleMoves, dFMoves, propSeemsWin)
    }

//
//    val terminationConditionAdvancementTypeMap = Map(
//      TA -> DAB,
//      TC -> DC,
//      TS -> DS
//    )
//
//    // TODO: add lazy here
//
//    val propSeemsWin = proponentSeemsToBeWinning(framework, dState, terminationCriteriaType)
//    val oppSeemsWin = !propSeemsWin
//
//    val advancementType = terminationConditionAdvancementTypeMap(terminationCriteriaType)
//
//    val opponentsConditionsMoves = List(OB1, OB2, OF2).flatMap(mType => Move(mType).isPossible(advancementType)).forall(filterMoves)
//    val proponentConditionMoves = List(PB1, PB2, PF2).flatMap(mType => Move(mType).isPossible(advancementType)).forall(filterMoves)
//
//
////    val (propTCCond, oppTCCond) = if (terminationCriteriaType == TC) {
////      (Move(PF1).isPossible(DC).isEmpty,
////      Move(OB2).isPossible(DC).isEmpty)
////    } else (true, true)
//
//    val tcCond = if (terminationCriteriaType == TC) {
//      !anyMovePossible
//    } else true
//
//    val opponentsAdditionalConditionsMoves = List(OF1, OF2).flatMap(mType => Move(mType).isPossible(DF)).forall(filterMoves)
//    val proponentsAdditionalConditionsMoves = List(PF1, PF2).flatMap(mType => Move(mType).isPossible(DF)).forall(filterMoves)
//
//    // TODO: temporary
//    val tsCond = if (terminationCriteriaType == TS) {
//      List(PF1, PF2, PB1, PB2).flatMap(mType => Move(mType).isPossible(DS)).forall(filterMoves)
//    } else false
//
//    // TODO: temporary
//    // TODO: most likely here are errors!!!
//
//
//    // TODO:
//    if (propSeemsWin && tcCond && (opponentsConditionsMoves || opponentsAdditionalConditionsMoves)) Some(true)
//    else if (oppSeemsWin && tcCond && tsCond && (proponentConditionMoves || proponentsAdditionalConditionsMoves)) Some(false)
//    else None
  }

}