package aba.reasoner.automatic2.movesPreferenceBased

import scala.util.Random

import aba.reasoner.{PotentialMove2, PotentialRuleMove}

object RuleHeadChoice extends Enumeration {

  type RuleHeadChoiceType = Value

  val
    MostRules, // m
    LeastRules, // l
    RandomHead // r
    = Value

  def apply(ruleHeadChoiceType: RuleHeadChoiceType)(implicit ruleMoves: Seq[PotentialMove2]): Seq[PotentialMove2] = {

    implicit val movesGrouped: Map[String, Seq[PotentialMove2]] = ruleMoves.groupBy{ case m: PotentialRuleMove => m.rule.head }

    ruleHeadChoiceType match {
      case MostRules => mostRules
      case LeastRules => leastRules
      case RandomHead => randomHead
    }
  }

  private def randomHead(implicit movesGrouped: Map[String, Seq[PotentialMove2]]): Seq[PotentialMove2] = {
    val randomRuleHead = Random.shuffle(movesGrouped.keys).head
    movesGrouped(randomRuleHead)
  }


  private def mostRules(implicit movesGrouped: Map[String, Seq[PotentialMove2]]): Seq[PotentialMove2] = {
    movesGrouped.maxBy(_._2.size)._2
  }


  private def leastRules(implicit movesGrouped: Map[String, Seq[PotentialMove2]]): Seq[PotentialMove2] = {
    movesGrouped.minBy(_._2.size)._2
  }
}
