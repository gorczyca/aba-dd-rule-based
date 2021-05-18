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

  // TODO:
  override def toString: String = super.toString

}
