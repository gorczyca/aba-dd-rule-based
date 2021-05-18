package aba.reasoner

import aba.framework.Literal
import aba.move.Move.MoveType


object DisputeState {
  // this factory method was created because the class constructor has execute "this" as the first statement
  def apply(move: MoveType, t: Set[Argument])(implicit previousState: DisputeState): DisputeState = {

    if (move.isProponentMove) new DisputeState(previousState.id + 1, Some(move), previousState.p ++ t, previousState.b ++ t)
    else new DisputeState(previousState.id + 1, Some(move), previousState.p, previousState.b ++ t)
  }
}


case class DisputeState(id: Int,
                        move: Option[MoveType],
                        p: Set[Argument],
                        b: Set[Argument]
                       ) {

  def this(goals: Set[Literal]) = this(0, None, goals.map(LiteralArgument), Set.empty)

  def bRuleArgs: Set[RuleArgument] = b.collect { case ruleArg: RuleArgument => ruleArg }

  def bLitArgs: Set[LiteralArgument] = b.collect { case litArg: LiteralArgument => litArg }

  def pRuleArgs: Set[RuleArgument] = p.collect { case ruleArg: RuleArgument => ruleArg }

  def pLitArgs: Set[LiteralArgument] = p.collect { case litArg: LiteralArgument => litArg }

}
