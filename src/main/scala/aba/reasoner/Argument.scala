package aba.reasoner

import aba.framework.{Literal, Rule}

abstract class Argument {
  def bParents(implicit dState: DisputeState): Set[Argument]
  def pParents(implicit dState: DisputeState): Set[Argument]
}

case class LiteralArgument(lit: Literal) extends Argument {
  override def bParents(implicit dState: DisputeState): Set[Argument] =
    dState.b.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.head == this.lit).toSet[Argument]

  override def pParents(implicit dState: DisputeState): Set[Argument] =
    dState.p.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.head == this.lit).toSet[Argument]
}


case class RuleArgument(rule: Rule) extends Argument {
  override def bParents(implicit dState: DisputeState): Set[Argument] =
    dState.b.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.body.contains(litArg.lit)).toSet[Argument]

  override def pParents(implicit dState: DisputeState): Set[Argument] =
    dState.p.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.body.contains(litArg.lit)).toSet[Argument]
}

