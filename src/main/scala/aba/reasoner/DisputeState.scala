package aba.reasoner

import aba.framework.Rule
import aba.move.Move.MoveType


object DisputeState {
  // this factory method was created because the class constructor has execute "this" as the first statement
  @deprecated
  def apply(move: MoveType, t: Set[Argument], argument: Argument)(implicit previousState: DisputeState): DisputeState = {
    if (move.isProponentMove) new DisputeState(Set.empty, Set.empty, Set.empty, Set.empty, previousState.id + 1, Some(move), previousState.p ++ t, previousState.b ++ t, Some(argument))
    else new DisputeState(Set.empty, Set.empty, Set.empty, Set.empty, previousState.id + 1, Some(move), previousState.p, previousState.b ++ t, Some(argument))
  }

  def apply(goals: Set[String]): DisputeState = {
    new DisputeState(goals, Set.empty, Set.empty, Set.empty)
  }

  // TODO: in the end this will just be the main constructor
  def apply(pStatements: Set[String], pRules: Set[Rule], oStatements: Set[String], oRules: Set[Rule]): DisputeState = {
    new DisputeState(pStatements, pRules, oStatements, oRules)
  }

}

case class DisputeState(pStatements: Set[String],
                        pRules: Set[Rule],
                        oStatements: Set[String],
                        oRules: Set[Rule],
                        id: Int = 0, // TODO: remove
                        move: Option[MoveType] = None, // TODO: remove
                        p: Set[Argument] = Set.empty, // TODO: remove
                        b: Set[Argument] = Set.empty, // TODO: remove
                        argument: Option[Argument] = None // TODO: remove
                       ) {


  def bRules: Set[Rule] = pRules ++ oRules
  def bStatements: Set[String] = pStatements ++ oStatements


  @deprecated
  def bRuleArgs: Set[RuleArgument] = b.collect { case ruleArg: RuleArgument => ruleArg }

  @deprecated
  def bLitArgs: Set[LiteralArgument] = b.collect { case litArg: LiteralArgument => litArg }

  @deprecated
  def pRuleArgs: Set[RuleArgument] = p.collect { case ruleArg: RuleArgument => ruleArg }

  @deprecated
  def pLitArgs: Set[LiteralArgument] = p.collect { case litArg: LiteralArgument => litArg }


  override def toString: String = s"P: ${(pStatements ++ pRules).mkString("; ")}\nB\\P: ${(oStatements ++ oRules).mkString("; ")}"

  @deprecated
  def sequenceElement: String = s"$id. ${ move match {
    case Some(value) => s"$value: ${ argument match {
      case Some(arg) => s"$arg"
      case _ => ""
    }}"
    case _ => "(init)"
  }}"
}
