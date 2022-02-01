package aba.reasoner

import aba.framework.Literal
import aba.move.Move.MoveType


object DisputeState {
  // this factory method was created because the class constructor has execute "this" as the first statement
  def apply(move: MoveType, t: Set[Argument], argument: Argument)(implicit previousState: DisputeState): DisputeState = {

    if (move.isProponentMove) new DisputeState(previousState.id + 1, Some(move), previousState.p ++ t, previousState.b ++ t, Some(argument))
    else new DisputeState(previousState.id + 1, Some(move), previousState.p, previousState.b ++ t, Some(argument))
  }

  def apply(goals: Set[Literal]): DisputeState = {
    val goalsArgs = goals.map(LiteralArgument).toSet[Argument] // TODO:
    new DisputeState(0, None, goalsArgs, goalsArgs, None)
  }

}

case class DisputeState(id: Int,
                        move: Option[MoveType],
                        p: Set[Argument],
                        b: Set[Argument],
                        argument: Option[Argument]
                       ) {

  def bRuleArgs: Set[RuleArgument] = b.collect { case ruleArg: RuleArgument => ruleArg }

  def bLitArgs: Set[LiteralArgument] = b.collect { case litArg: LiteralArgument => litArg }

  def pRuleArgs: Set[RuleArgument] = p.collect { case ruleArg: RuleArgument => ruleArg }

  def pLitArgs: Set[LiteralArgument] = p.collect { case litArg: LiteralArgument => litArg }

  override def toString: String = s"P: ${p.mkString("; ")}\nB\\P: ${b.diff(p).mkString("; ")}"

  def sequenceElement: String = s"$id. ${ move match {
    case Some(value) => s"$value: ${ argument match {
      case Some(arg) => s"$arg"
      case _ => ""
    }}"
    case _ => "(init)"
  }}"
}
