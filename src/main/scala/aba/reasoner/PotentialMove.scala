package aba.reasoner

import aba.move.Move.MoveType


case class PotentialMove(ruleArgument: Option[RuleArgument],
                         // literalArgument: Option[LiteralArgument],
                         assumptionArgument: Option[LiteralArgument],
                         literalArguments: Set[LiteralArgument],
                         moveType: MoveType,
                         additionalInfo: Option[String]) extends Ordered[PotentialMove] {
  override def compare(that: PotentialMove): Int = this.moveType compare that.moveType

  def perform(implicit dState: DisputeState): DisputeState = {

    val totalArguments: Set[Argument] = ruleArgument match {
      case Some(ruleArg) => literalArguments.toSet[Argument] + ruleArg
      case None => literalArguments.toSet[Argument]   // TODO: do it better than toSet[Argument]
    }

    val arg = (ruleArgument, assumptionArgument) match {
      case (Some(_), Some(_)) => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
      case (Some(ruleArg), None) => ruleArg
      case (None, Some(litArg)) => litArg
      case _ => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
    }

    DisputeState(moveType, totalArguments, arg)
  }


  override def toString: String = {

    // TODO: remove?
//    val additionalInfoStr = additionalInfo match {
//      case Some(addInfo) => s" ($addInfo)"
//      case _ => ""
//    }

//    val ruleArgStr = ruleArgument match {
//      case Some(ruleArg) => s"Rule: $ruleArg"
//      case _ => ""
//    }

    // TODO? previously was showing all literal arguments, probably just remove
    //val litArgStr = if (literalArguments.isEmpty) "" else s"Literals: ${literalArguments.mkString(",")}"

    //s"$moveType$additionalInfoStr: $ruleArgStr $litArgStr"

    (ruleArgument, assumptionArgument) match {
      case (Some(_), Some(_)) => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
      case (Some(ruleArg), None) => s"Rule: $ruleArg"
      case (None, Some(litArg)) => s"Assumption: $litArg"
      case _ => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
    }

  }



}
