package aba.reasoner.automatic

import aba.framework.Framework
import aba.move.{DisputeAdvancement, PB2Move, PF2Move, TerminationCriteria}
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.TerminationCriteriaType
import aba.reasoner.{DisputeState, PotentialMove, PotentialMove2, RuleArgument}

import scala.annotation.tailrec

class AutoPreferenceReasoner(dfs: Boolean) {

  val prioritizeProp: Boolean = false

  val propPreference: Seq[MoveType] = Seq(PF1, PB1, PB2, PF2)
  val oppPreference: Seq[MoveType] = Seq(OB1, OB2, OF2)


  private def generateNewDisputeStates(implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                                       framework: Framework,
                                       dStateAuto: DisputeStateAuto) : List[DisputeStateAuto] = {


    // TODO: this filtering do down there. And already if there will be some moves filtered out, it will be easier to decide the termination
    val possMovesModified1 = if (possibleMoves.contains(PB2)) {
      val newPB2 = possibleMoves(PB2).filter {
        case PB2Move(rule, Some(attacked), _) => !attacked.subsetOf(dStateAuto.ignoredCulpritCandidates)
        case _ => true
      }
      if (newPB2.isEmpty) possibleMoves - PB2
      else possibleMoves + (PB2 -> newPB2)
    } else possibleMoves

    val newPossMoves = if (possMovesModified1.contains(PF2)) {
      val newPF2 = possMovesModified1(PF2).filter {
        // not attacking anything, just trying to add assArg to P
            // TODO: catching multiple traits? PF2 attacking move?
        case PF2Move(asm, None, _) => !dStateAuto.ignoredProponentAssumptions.contains(asm)
        // attacking, check if attacked not added to ignored
        case PF2Move(_, Some(attacked), _) => !attacked.subsetOf(dStateAuto.ignoredCulpritCandidates)
        case _ => true
      }
      if (newPF2.isEmpty) possMovesModified1 - PF2
      else possMovesModified1 + (PF2 -> newPF2)
    } else possMovesModified1

    // now newPossMoves contain all moves without ignored ones
    val (propMoves, oppMoves) = newPossMoves.partition(_._1.isProponentMove)

    val chosenMoves =
      if (prioritizeProp)
        if (propMoves.nonEmpty) propMoves
        else oppMoves
      else if (oppMoves.nonEmpty) oppMoves
           else (propMoves)

    if (chosenMoves == propMoves) {
      // proponent has been chosen
      val chosenMoveType = propMoves.toSeq.minBy { case (moveType, _) => propPreference.indexOf(moveType) } // minBy = sortBy {} . head

      chosenMoveType match {
        case (PB1, movesSeq) =>
          // backward moves. We need to choose a sentence to backward expand and add to stack all of them
          movesSeq.groupBy {
            case PotentialMove(Some(RuleArgument(rule)), None, _, PB1, _, _) => rule.head
          }.head._2.map(potMove => DisputeStateAuto(potMove, dStateAuto, increaseBranchingLevel = false)).toList
        case (PB2, moveSeq) =>
          // attack moves. We need to choose which one to attack
          val potMoves = moveSeq.groupBy {
            case PotentialMove(Some(RuleArgument(rule)), None, _, PB2, _, Some(attacked)) => rule.head
          }.head._2
          val ignoredCulpritCandidates = potMoves.head.attacking.get  // I can be sure that it attacks something
          potMoves.map(potMove => DisputeStateAuto(potMove, dStateAuto, increaseBranchingLevel = false)).toList :+ // add the ignoring move
            DisputeStateAuto(dStateAuto.dState, dStateAuto.ignoredCulpritCandidates ++ ignoredCulpritCandidates, dStateAuto.ignoredProponentAssumptions, dStateAuto.performedArguments)
        case (PF2, moveSeq) =>
          val (moveSeqAttacking, moveSetNonAttacking) = moveSeq.partition { case PotentialMove(_, _, _, _, _, attackingOpt) => attackingOpt match {
            case Some(_) => true
            case _ => false
          }}

          // here somehow define which one we prefer
          val chosenSet = if (moveSeqAttacking.nonEmpty) moveSeqAttacking else moveSetNonAttacking

          if (chosenSet == moveSeqAttacking) {
            val attackingMoves = moveSeqAttacking.groupBy(_.attacking.get).head
            attackingMoves._2.map(potMove => DisputeStateAuto(potMove, dStateAuto, increaseBranchingLevel = false)) :+ // ignoring move
              DisputeStateAuto(dStateAuto.dState, dStateAuto.ignoredCulpritCandidates ++ attackingMoves._1, dStateAuto.ignoredProponentAssumptions, dStateAuto.performedArguments)
          }.toList  else {
            // non attacking move = just simply add or do ignore adding
            val assMove = moveSetNonAttacking.head
            DisputeStateAuto(assMove, dStateAuto, increaseBranchingLevel = false) :: DisputeStateAuto(dStateAuto.dState, dStateAuto.ignoredCulpritCandidates, dStateAuto.ignoredProponentAssumptions + assMove.assumptionArgument.get.lit, dStateAuto.performedArguments) :: Nil
          }
        case (PF1, moveSeq) => DisputeStateAuto(moveSeq.head, dStateAuto, increaseBranchingLevel = false) :: Nil
      }
    } else {
      // opponent Moves. We will just perform it
      DisputeStateAuto(oppMoves.toList.minBy { case (moveType, _) => oppPreference.indexOf(moveType) }._2.head, dStateAuto, increaseBranchingLevel = false) :: Nil
    }

  }


  @tailrec
  final def getNewIncompleteSuccessfulDSAndStackRec(stack: List[DisputeStateAuto],
                                                    successfulDS: List[DisputeStateAuto],
                                                    startTime: Option[Long] = None)
                                                   (implicit framework: Framework,
                                                    onlyOne: Boolean,
                                                    timeoutOpt: Option[Int],
                                                    tCriteria: TerminationCriteriaType,
                                                    dAdvancementType: DisputeAdvancementType): (List[DisputeStateAuto], List[DisputeStateAuto], Boolean, Double) = {

    val sTime = startTime match {
      case Some(t) => t
      case _ => System.nanoTime()
    }

    val duration = (System.nanoTime - sTime) / 1e9d

    timeoutOpt match {
      case Some(timeout) if (duration > timeout)  => (stack, successfulDS, true, duration)
      case _ =>


        if (stack.isEmpty || (onlyOne && successfulDS.nonEmpty)) (stack, successfulDS, false, duration)
        else {
          implicit val headDS :: restOfDisputeStates = stack // TODO: type annotation?
          implicit val headDState: DisputeState = headDS.dState
          implicit val possibleMoves: Map[MoveType, Seq[PotentialMove]] = DisputeAdvancement(dAdvancementType).getPossibleMoves

          val propPerformedMovesCount = headDS.performedArguments.count(_.moveType.isProponentMove)

          if (headDS.performedArguments.isEmpty) {
            println("Initial:")
            println(0 + ": " + headDS.dState.p.mkString("; "))
          } else {
            val allPerformedMovesCount = headDS.performedArguments.size
            println(allPerformedMovesCount +":" + "\t" * propPerformedMovesCount + headDS.performedArguments.last.toString +  "\t" + s""" icc: { ${headDS.ignoredCulpritCandidates.mkString(";")} } ia: { ${headDS.ignoredProponentAssumptions.mkString(";")} }""")
          }


          TerminationCriteria.checkIfOver(tCriteria) match {
            case Some(true) =>
              println("\t" * propPerformedMovesCount + "SUCCESS!")
              getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS :+ headDS, Some(sTime))
            case Some(false) =>
              println("\t" * propPerformedMovesCount + "FAIL!")
              getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS, Some(sTime))
            case None =>

              // it is possible that the only PossibleMoves are to attack an ignored culprit candidate
              // OR PF1 moves with assumptions chosen as ignored
              if (possibleMoves.values.flatten.forall {
                // attack using a rule case
                case PotentialMove(Some(_), None, _, PB2, _, Some(attackedLits)) => attackedLits.subsetOf(headDS.ignoredCulpritCandidates)
                // attack using an assumption
                case PotentialMove(None, Some(_), _, PF2, _, Some(attackedLits)) => attackedLits.subsetOf(headDS.ignoredCulpritCandidates)
                // None are attacked - meaning that this move should add the assArg to P
                case PotentialMove(None, Some(assArg), _, PF2, _, None) => headDS.ignoredProponentAssumptions.contains(assArg.lit)
                //
                case _ => false
              }) {
                println("\t" * propPerformedMovesCount + "FAIL! (only ignored moves left)")
                getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS, Some(sTime))
              } else {

                // ...
                val newDisputeStates = generateNewDisputeStates
                val disputeStates = if (dfs) newDisputeStates ++ restOfDisputeStates else restOfDisputeStates ++ newDisputeStates
                getNewIncompleteSuccessfulDSAndStackRec(disputeStates, successfulDS, Some(sTime))
              }
          }
        }
    }
  }
}
