package aba.reasoner.approximate

import aba.framework.{Framework, Rule}
import aba.move.Move.MoveType
import aba.reasoner.automatic2.movesPreferenceBased.MovesPreferenceBasedAutomaticReasoner2
import aba.reasoner.{PotentialMove2, PotentialRuleMove}
import aba.reasoner.automatic2.DisputeStateAuto2

import scala.util.Random

object ApproximateReasoner {
  def default(automaticReasoner2: MovesPreferenceBasedAutomaticReasoner2)(implicit framework: Framework): ApproximateReasoner = {
    new ApproximateReasoner(
      automaticReasoner2,
      proponentP = 1.0,
      opponentP = 1.0,
      framework)
  } // TODO: more options
}

case class ApproximateReasoner(automaticReasoner2: MovesPreferenceBasedAutomaticReasoner2, // TODO:
                               proponentP: Double = 1.0,
                               opponentP: Double = 1.0,
                               framework: Framework,
                               static: Boolean = true
                               ){

  private val proponentRules: Set[Rule] = getRulesSample(proponentP, framework.rules)
  private val opponentRules: Set[Rule] = getRulesSample(opponentP, framework.rules)

  private def getRulesSample(p: Double, rules: Set[Rule]): Set[Rule] = {
    if (p == 1.0) rules // TODO: maybe that's unnecessary // TODO: either probabilistically OR just take a subset?
    else {
      rules.filter { _ => Random.nextDouble() <= p }
    }
  }

  val approximateFiltering: (PotentialMove2 => Boolean, Map[MoveType, Seq[PotentialMove2]]) => PotentialMove2 => Boolean = (filterIgnoredMoves: PotentialMove2 => Boolean, posMoves: Map[MoveType, Seq[PotentialMove2]]) => {

    case ruleMove: PotentialRuleMove =>
      if (ruleMove.moveType.isProponentMove) proponentRules.contains(ruleMove.rule) && !filterIgnoredMoves(ruleMove)
      else opponentRules.contains(ruleMove.rule) && !filterIgnoredMoves(ruleMove)

    case m => !filterIgnoredMoves(m)
  }


  private def getRandomBoolean(p: Double): Boolean = if (p == 1.0) true else (Random.nextDouble() <= p)


  val getNewFilterFunction: (PotentialMove2 => Boolean, Map[MoveType, Seq[PotentialMove2]]) => PotentialMove2 => Boolean = (filterIgnoredMoves: PotentialMove2 => Boolean, posMoves: Map[MoveType, Seq[PotentialMove2]]) => {

    val possibleMovesFiltered: Map[MoveType, Seq[PotentialMove2]] = posMoves.map {
      case (mType, moves) => (mType, moves.filterNot(filterIgnoredMoves)) // filter out the ignored moves
    }.filter { case (_, moves) => moves.nonEmpty } // if empty move type, remove entirely

    val proponentRules = possibleMovesFiltered.filter(_._1.isProponentMove).flatMap(_._2).filter{
      case _: PotentialRuleMove => true
      case _ => false
    }.asInstanceOf[Iterable[PotentialRuleMove]].map(_.rule).toSet

    val opponentRules = possibleMovesFiltered.filter(_._1.isOpponentsMove).flatMap(_._2).filter{
      case _: PotentialRuleMove => true
      case _ => false
    }.asInstanceOf[Iterable[PotentialRuleMove]].map(_.rule).toSet

    val propRuleSample = getRulesSample(proponentP, proponentRules)
    val oppRuleSample = getRulesSample(opponentP, opponentRules)

    val filteringFunction: PotentialMove2 => Boolean = {
      case ruleMove: PotentialRuleMove =>
        if (ruleMove.moveType.isProponentMove) propRuleSample.contains(ruleMove.rule) && !filterIgnoredMoves(ruleMove)
        else oppRuleSample.contains(ruleMove.rule) && !filterIgnoredMoves(ruleMove)
      case otherMove => !filterIgnoredMoves(otherMove)
    }

    filteringFunction
  }


  def getNewIncompleteSuccessfulDSAndStackRec(stack: List[DisputeStateAuto2],
                                              successfulDS: List[DisputeStateAuto2],
                                              startTimeOpt: Option[Long] = None)
                                                   (implicit framework: Framework,
                                                    onlyOne: Boolean,
                                                    timeoutOpt: Option[Int] = None): (List[DisputeStateAuto2], List[DisputeStateAuto2], Boolean, Double) = {


    val filteringFunction = if (static)  approximateFiltering
    else getNewFilterFunction


    automaticReasoner2.getNewIncompleteSuccessfulDSAndStackRec(stack, successfulDS, startTimeOpt, additionalFilter = filteringFunction)
  }
}
