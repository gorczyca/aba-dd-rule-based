package aba.reasoner.automatic2.complex

import aba.framework.Framework
import aba.reasoner.automatic2.{AutomaticReasoner2, DisputeStateAuto2}

import scala.annotation.tailrec

class GroundedReasoner(automaticReasoner: AutomaticReasoner2) extends ComplexReasoner(automaticReasoner) {

  private def getNewAssumptionsStack(n: Int)(implicit framework: Framework): List[Set[String]] =
    (framework.assumptions.toList combinations n).map(_.toSet).toList


  @tailrec
  final def findGroundedDisputeDerivations(n: Int,
                                           assumptionsStack: List[Set[String]],
                                           startTimeOpt: Option[Long] = None,
                                           timeoutOpt: Option[Int] = None)
                                          (implicit framework: Framework,
                                           onlyOne: Boolean): (List[DisputeStateAuto2], Boolean, Double) = {


    // TODO: move all this to some function

    val sTime = startTimeOpt match {
      case Some(t) => t
      case _ => System.nanoTime()
    }

    val duration = (System.nanoTime - sTime) / 1e9d

    // stop the search if timeout has expired
    timeoutOpt match {
      case Some(timeout) if duration > timeout =>
        return (Nil, true, duration) // break the search
      case _ =>
    }

    // TODO: until this

    assumptionsStack match {
      case subsetHead::restStack =>
        val constraints = framework.assumptions -- subsetHead
        val newSuccessfulDDs = findSuccessfullCompleteDisputeDerivations(constraints, onlyOne = onlyOne)

        if (newSuccessfulDDs.isEmpty) {

          //println(f"${subsetHead.size}: FAIL: ${subsetHead.mkString(",")}")

          findGroundedDisputeDerivations(n, restStack, Some(sTime), timeoutOpt)
        }
        else (newSuccessfulDDs, false, duration)

      case Nil =>
        if (n >= framework.assumptions.size) (Nil, false, duration)
        else {
          val newStack = getNewAssumptionsStack(n+1)
          findGroundedDisputeDerivations(n+1, newStack, Some(sTime), timeoutOpt)
        }
    }
  }
}
