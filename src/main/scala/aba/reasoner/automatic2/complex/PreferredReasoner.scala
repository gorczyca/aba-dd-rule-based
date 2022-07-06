package aba.reasoner.automatic2.complex

import aba.framework.Framework
import aba.reasoner.DisputeState
import aba.reasoner.automatic2.{AutomaticReasoner2, DisputeStateAuto2}

import scala.annotation.tailrec

class PreferredReasoner(automaticReasoner: AutomaticReasoner2) extends ComplexReasoner(automaticReasoner) {


  private def getNewAssumptionsStack(failedUnblockedAssumptions: Set[Set[String]],
                                     successfulUnblockedAssumptions: Set[Set[String]]): List[Set[String]] = {

    val subsets = failedUnblockedAssumptions.filter(_.nonEmpty).flatMap(set => set.toList.combinations(set.size - 1).map(_.toSet))
    val subsets2 = subsets -- failedUnblockedAssumptions
    //val filteredSubsets = subsets2.filter(set => !successfulUnblockedAssumptions.exists(succSet => set.subsetOf(succSet)))


    //filteredSubsets.toList
    subsets2.toList
  }


  @tailrec
  final def findSuccessfulDDs(unblockedAssumptionsStack: List[Set[String]],
                        successfulDDs: List[DisputeStateAuto2],
                        failedUnblockedAssumptions: Set[Set[String]],
                        successfulUnblockedAssumptions: Set[Set[String]],
                        goals: Set[String],
                        onlyOne: Boolean = false,
                        startTimeOpt: Option[Long] = None,
                        timeoutOpt: Option[Int] = None)
                             (implicit framework: Framework): (List[DisputeStateAuto2], Boolean, Double) = {


    val sTime = startTimeOpt match {
      case Some(t) => t
      case _ => System.nanoTime()
    }

    val duration = (System.nanoTime - sTime) / 1e9d

    // stop the search if timeout has expired
    timeoutOpt match {
      case Some(timeout) if duration > timeout =>
        return (successfulDDs, true, duration) // break the search
      case _ =>
    }

    // stop the search if stack is empty OR there is some successfulDS found and only one was required
    if (onlyOne && successfulDDs.nonEmpty)
      return (successfulDDs, false, duration)



    unblockedAssumptionsStack match {
      case subsetHead::restStack =>
        val constraints = framework.assumptions -- subsetHead
        val newSuccessfulDDs = findSuccessfullCompleteDisputeDerivations(constraints, onlyOne = onlyOne)

        val size = subsetHead.size
        //println(size)

        //if (size != pSize) { println(size) }




        if (newSuccessfulDDs.isEmpty) {
          //println(f"$size: FAIL: ${subsetHead.mkString(",")}")
          findSuccessfulDDs(restStack, successfulDDs, failedUnblockedAssumptions + subsetHead, successfulUnblockedAssumptions, goals, onlyOne, Some(sTime), timeoutOpt)
        }
        else {
           //println(f"$size: SUCCESS: ${subsetHead.mkString(",")}
            val successfulDDsSupportingGoals = newSuccessfulDDs.filter(s => goals.subsetOf(s.dState.pPlayedCompleteStatements))
            findSuccessfulDDs(restStack, successfulDDs ++ successfulDDsSupportingGoals, failedUnblockedAssumptions, successfulUnblockedAssumptions + subsetHead, goals, onlyOne, Some(sTime), timeoutOpt)
        }

      case Nil =>
        val newSubsets = getNewAssumptionsStack(failedUnblockedAssumptions, successfulUnblockedAssumptions)
        if (newSubsets.nonEmpty) findSuccessfulDDs(newSubsets, successfulDDs, Set.empty, successfulUnblockedAssumptions, goals, onlyOne, Some(sTime), timeoutOpt)
        else (successfulDDs, false, duration)
    }
  }

}
