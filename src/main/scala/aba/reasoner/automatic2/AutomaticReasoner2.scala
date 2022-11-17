package aba.reasoner.automatic2

import aba.framework.{Framework, Rule}
import aba.move.DisputeAdvancement.{DAB, DABF, DisputeAdvancementType}
import aba.move.Move.MoveType
import aba.move.{DisputeAdvancement, Move, OB1Move, OB2Move, OF1Move, OF2Move, PB1Move, PB2Move, PF1Move, PF2Move, TerminationCriteria}
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.{DisputeState, PotentialMove2}

import java.io.FileWriter
import scala.annotation.tailrec
import scala.util.Random


abstract class AutomaticReasoner2(dfs: Boolean,
                                  dAdvancementType: DisputeAdvancementType,
                                  tCriteriaType: TerminationCriteriaType,
                                  startWithAdmissible: Boolean) {


    val initialTCAndDA: (TerminationCriteriaType, DisputeAdvancementType) = {
        if (startWithAdmissible) {
            (tCriteriaType, dAdvancementType) match {
                // TODO: this now only allows to combine corresponding advancement types / termination criteria
                case (_, DAB | DABF) => (tCriteriaType, dAdvancementType)
                case _ => (TA, DABF)
            }
        } else (tCriteriaType, dAdvancementType)
    }



    // subclasses need to override this
    protected def generateNewDisputeStates(implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                                 framework: Framework,
                                 dStateAuto: DisputeStateAuto2) : List[DisputeStateAuto2]

    @tailrec
    final def getNewIncompleteSuccessfulDSAndStackRec(stack: List[DisputeStateAuto2],
                                                      successfulDS: List[DisputeStateAuto2],
                                                      startTimeOpt: Option[Long] = None,
                                                      additionalFilter: (PotentialMove2 => Boolean, Map[MoveType, Seq[PotentialMove2]]) => (PotentialMove2 => Boolean) = (isIgnored, _) => (x: PotentialMove2) => !isIgnored(x),
                                                      movesNumberOpt: Option[Int] = None)
                                           (implicit framework: Framework,
                                            onlyOne: Boolean,
                                            timeoutOpt: Option[Int]): (List[DisputeStateAuto2], List[DisputeStateAuto2], Boolean, Double) = {

        val printDebug = false
        val outputToFile = false

        val sTime = startTimeOpt match {
            case Some(t) => t
            case _ => System.nanoTime()
        }





        val duration = (System.nanoTime - sTime) / 1e9d

        // stop the search if timeout has expired
        timeoutOpt match {
            case Some(timeout) if duration > timeout =>
                return (stack, successfulDS, true, duration) // break the search
            case _ =>
        }

        movesNumberOpt match {
            case Some(0)  =>
                // in this case just return
                return (stack, Nil, false, duration)
            case _ =>
        }





        // stop the search if stack is empty OR there is some successfulDS found and only one was required
        if (stack.isEmpty || (onlyOne && successfulDS.nonEmpty) || (successfulDS.nonEmpty && movesNumberOpt.isDefined))
            return (stack, successfulDS, false, duration)


        implicit val headDS :: restOfDisputeStates = stack // TODO: type annotation?
        implicit val headDState: DisputeState = headDS.dState

        val currentTC = headDS.currentTerminationCriteria
        val currentAT = headDS.currentAdvancementType

        val possibleMoves = DisputeAdvancement(currentAT).getPossibleMoves

        //val size = successfulDS.size + 1
        //println(s"${size}: Stack size: ${stack.size}")
        //val branchingDepth = headDS.performedMoves.count(_.moveType.isProponentMove)
        //val performedMoveStr = (headDS.performedMoves.lastOption match {
        //    case Some(lastMove) => lastMove.toString
        //    case _ => ""
        //}) + " | igP: " + headDS.ignoredProponentAssumptions.mkString(",") + " | igC: " + headDS.ignoredCulpritCandidates.mkString(",") +  " | igO: | " + headDS.ignoredCurrentlyDefendedAssumptions.mkString(",") + "\n"

        //if (outputToFile) appendToFile(("\t" * branchingDepth) + performedMoveStr)
        //if (printDebug) print(size + ("\t" * branchingDepth) + performedMoveStr)

        val filterMoves = additionalFilter(isIgnoredMove, possibleMoves)


        //val filterMoves: PotentialMove2 => Boolean = move => additionalFilter(move) && !isIgnoredMove(move)

        //if (debugCondition) println("debug condition")

        // todo: temporary



        implicit val possibleMovesFiltered: Map[MoveType, Seq[PotentialMove2]] = possibleMoves.map {
            case (mType, moves) => (mType, moves.filter(filterMoves)) // filter out the ignored moves
        }.filter { case (_, moves) => moves.nonEmpty } // if empty move type, remove entirely


        TerminationCriteria.checkIfOver(currentAT, currentTC, filterMoves = filterMoves) match {
            case Some(true) => //

                // check if currentTA/DC is the target one
                if (currentAT == dAdvancementType) {


//                    val currDefences = headDState.defences
//                    val defencesUpToDate = successfulDS.map(_.dState.defences).toSet
//
                    val successfulDerivations = successfulDS :+ headDS
//                    val defences = successfulDerivations.map(_.dState.defences).toSet
//
//                    if (!defencesUpToDate.contains(currDefences)) {
//
//                        println(s"Found: ${successfulDerivations.size}, Distinct defences: ${defences.size}")
//                        println(s"Defences: { ${headDState.defences.toList.sorted.mkString(",")} } " +
//                          s"\n igP { ${headDS.ignoredProponentAssumptions.toList.sorted.mkString(",")} } " +
//                          s"\n igC { ${headDS.ignoredCulpritCandidates.toList.sorted.mkString(",")} } " +
//                          s"\n igO { ${headDS.ignoredCurrentlyDefendedAssumptions.toList.sorted.mkString(",")} } ")
//
//                    }

                    // TODO: temporary
                    //if (outputToFile) appendToFile(("\t" * (branchingDepth + 1)) + s"YES\n")
                    //if (printDebug) print(size + ("\t" * (branchingDepth + 1)) + s"YES\n")


                    // it is actually the one that we wanted
                    getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDerivations, Some(sTime), additionalFilter = additionalFilter, movesNumberOpt)

                } else {

                    // it is successful wrt DABF, TA
                    // now switch to the desired
//                    println("Found with TA, DABF")
//
//
//
//                    println(s"Defences: { ${headDState.defences.toList.sorted.mkString(",")} } " +
//                      s"\n igP { ${headDS.ignoredProponentAssumptions.toList.sorted.mkString(",")} } " +
//                      s"\n igC { ${headDS.ignoredCulpritCandidates.toList.sorted.mkString(",")} } " +
//                      s"\n igO { ${headDS.ignoredCurrentlyDefendedAssumptions.toList.sorted.mkString(",")} } ")

//                    val newMovesNumberOpt = movesNumberOpt match {
//                        case Some(0)  =>
//                            // in this case just return
//                            return (stack, Nil, false, duration)
//                        case Some(movesNumber) => Some(movesNumber - 1)
//                        case None => None
//                    }

                    val upgradedDS = headDS.copy(currentAdvancementType = dAdvancementType, currentTerminationCriteria = tCriteriaType)
                    getNewIncompleteSuccessfulDSAndStackRec(upgradedDS +: restOfDisputeStates, successfulDS, Some(sTime), additionalFilter = additionalFilter, movesNumberOpt)
                }



            case Some(false) =>
                //if (outputToFile) appendToFile(("\t" * (branchingDepth + 1)) + s"NO\n")
                //if (printDebug) print(size + ("\t" * (branchingDepth + 1)) + s"NO\n")

                getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS, Some(sTime), additionalFilter = additionalFilter, movesNumberOpt)
            case _ =>

                // TODO:
                if (possibleMovesFiltered.isEmpty) {
                    //if (outputToFile) appendToFile(("\t" * (branchingDepth + 1)) + s"NO\n")
                    //if (printDebug) print(("\t" * (branchingDepth + 1)) + s"NO\n")

                    getNewIncompleteSuccessfulDSAndStackRec(restOfDisputeStates, successfulDS, Some(sTime), additionalFilter = additionalFilter, movesNumberOpt)
                } else {

                    // continue with current dispute state -- it is not finished yet
                    val newDisputeStates = generateNewDisputeStates

//                    val nDisputeStatesSet = newDisputeStates.toSet
//                    val intersection1 = nDisputeStatesSet intersect consideredDStates
//                    // TODO:
//                    if (intersection1.nonEmpty) {
//                        println("Some state has already been considered")
//                    }

                    val newMovesNumberOpt = movesNumberOpt match {
                        case Some(movesNumber) => Some(movesNumber - 1)
                        case None => None
                    }

                    val disputeStates = if (dfs) newDisputeStates ++ restOfDisputeStates else restOfDisputeStates ++ newDisputeStates
                    getNewIncompleteSuccessfulDSAndStackRec(disputeStates, successfulDS, Some(sTime), additionalFilter = additionalFilter, newMovesNumberOpt)
//                    getNewIncompleteSuccessfulDSAndStackRec(disputeStates, successfulDS, Some(sTime), headDS.ignoredProponentAssumptions, headDS.ignoredCulpritCandidates, headDS.ignoredCurrentlyDefendedAssumptions, consideredDStates ++ nDisputeStatesSet)
                }
        }

    }

    protected def takeRandomMove(moves: Iterable[PotentialMove2]): PotentialMove2 = Random.shuffle(moves).head

    // TODO: change true / false. Like Not ignored => true
    private def isIgnoredMove(potMove: PotentialMove2)(implicit disputeStateAuto: DisputeStateAuto2): Boolean = {

        potMove match {

            // TODO: changed to subsetOf to intersect
// TODO: removed
//            case OB1Move(_, Some(attackedAssumptions)) => // we only allow for moves that attack defences
//                !(attackedAssumptions.intersect(disputeStateAuto.ignoredCurrentlyDefendedAssumptions) subsetOf
//                  disputeStateAuto.dState.defences)

//            case OB2Move(_, Some(attackedAssumptions)) =>
//                !(attackedAssumptions.intersect(disputeStateAuto.ignoredCurrentlyDefendedAssumptions) subsetOf
//                  disputeStateAuto.dState.defences)
//
//            case OF1Move(_, Some(attackedAssumptions)) =>
//                !(attackedAssumptions.intersect(disputeStateAuto.ignoredCurrentlyDefendedAssumptions) subsetOf
//                  disputeStateAuto.dState.defences)
//
//            case OF2Move(_, Some(attackedAssumptions)) =>
//                !(attackedAssumptions.intersect(disputeStateAuto.ignoredCurrentlyDefendedAssumptions) subsetOf
//                  disputeStateAuto.dState.defences)

            // todo, maybe OB1 move should also be here, not sure
            case PB1Move(Rule(_, _, body), None) =>
                (body intersect disputeStateAuto.ignoredProponentAssumptions).nonEmpty

            case PB1Move(Rule(_, _, body), Some(attackedAssumptions)) =>
                // contains assumptions we do not want to include
                (body intersect disputeStateAuto.ignoredProponentAssumptions).nonEmpty ||
                  // attacks assumptions we don't want to attack
                  attackedAssumptions.intersect(disputeStateAuto.ignoredCulpritCandidates).nonEmpty

                // TODO: this should never be the case
            case PB2Move(Rule(_, _, body), None) =>
                (body intersect disputeStateAuto.ignoredProponentAssumptions).nonEmpty

            case PB2Move(Rule(_, _, body), Some(attackedAssumptions)) =>
                // contains assumptions we do not want to include
                (body intersect disputeStateAuto.ignoredProponentAssumptions).nonEmpty ||
                // attacks assumptions we don't want to attack
                attackedAssumptions.intersect(disputeStateAuto.ignoredCulpritCandidates).nonEmpty

            case PF1Move(Rule(_, _, body), Some(attackedAssumptions)) =>
                // contains assumptions we do not want to include // TODO: should never be the case
                (body intersect disputeStateAuto.ignoredProponentAssumptions).nonEmpty ||
                // attacks assumptions we don't want to attack
                attackedAssumptions.intersect(disputeStateAuto.ignoredCulpritCandidates).nonEmpty

            case PF1Move(Rule(_, _, body), None) =>
                // contains assumptions we do not want to include // TODO: should never be the case
                (body intersect disputeStateAuto.ignoredProponentAssumptions).nonEmpty

            case PF2Move(asm, Some(attackedAssumptions)) =>
              // assumption we do not want to include
              disputeStateAuto.ignoredProponentAssumptions.contains(asm) ||
              // attacks assumptions we don't want to attack
              attackedAssumptions.intersect(disputeStateAuto.ignoredCulpritCandidates).nonEmpty

            case PF2Move(asm, None) =>
                // assumption we do not want to include
                disputeStateAuto.ignoredProponentAssumptions.contains(asm)

            case _ => false
        }

    }


    private def appendToFile(line: String): Unit = {
        val fw = new FileWriter("test.txt", true)
        try {
            fw.write(line)
        }
        finally fw.close()
    }
}