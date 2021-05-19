package aba.reasoner

import aba.move.Move.MoveType


case class PotentialMove(ruleArgument: Option[RuleArgument],
                         literalArguments: Set[LiteralArgument],
                         moveType: MoveType,
                         additionalInfo: Option[String]) extends Ordered[PotentialMove] {
  override def compare(that: PotentialMove): Int = this.moveType compare that.moveType

  def perform(implicit dState: DisputeState): DisputeState = {

    val totalArguments: Set[Argument] = ruleArgument match {
      case Some(ruleArg) => literalArguments.toSet[Argument] + ruleArg
      case None => literalArguments.toSet[Argument]   // TODO: do it better than toSet[Argument]
    }

    DisputeState(moveType, totalArguments)
  }


  override def toString: String = {

    // TODO:
    val additionalInfoStr = additionalInfo match {
      case Some(addInfo) => s" ($addInfo)"
      case _ => ""
    }

    val ruleArgStr = ruleArgument match {
      case Some(ruleArg) => s"Rule: $ruleArg"
      case _ => ""
    }

    val litArgStr = if (literalArguments.isEmpty) "" else s"Literals: ${literalArguments.mkString(",")}"

    s"$moveType$additionalInfoStr: $ruleArgStr $litArgStr"
  }



}
