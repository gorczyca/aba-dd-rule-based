package aba.reasoner

import aba.framework.{Literal, Rule}

abstract class Argument {
  def parents(implicit dState: DisputeState): Set[Argument]
  def pParents(implicit dState: DisputeState): Set[Argument] = parents intersect dState.p
  def children(implicit dState: DisputeState): Set[Argument] = dState.b.filter(_.parents.contains(this))
  def pChildren(implicit dState: DisputeState): Set[Argument] = children intersect dState.p
}

case class LiteralArgument(lit: Literal) extends Argument {

  // for preventing from having two same arguments (but different objects) in a set
  override def equals(obj: Any): Boolean = {
    obj match {
      case litArg: LiteralArgument => litArg.lit.equals(lit)
      case _ => false
    }
  }

  // same as above
  override def hashCode(): Int = lit.hashCode()

  override def parents(implicit dState: DisputeState): Set[Argument] =
    dState.b.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.head == this.lit).toSet[Argument]

//  override def pParents(implicit dState: DisputeState): Set[Argument] =
//    dState.p.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.head == this.lit).toSet[Argument]
}


case class RuleArgument(rule: Rule) extends Argument {

  //
  override def equals(obj: Any): Boolean = {
    obj match {
      case ruleArg: RuleArgument => ruleArg.rule.eq(rule)
      case _ => false
    }
  }

  //
  override def hashCode(): Int = rule.hashCode()

  override def parents(implicit dState: DisputeState): Set[Argument] =
    dState.b.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.body.contains(litArg.lit)).toSet[Argument]

//  override def pParents(implicit dState: DisputeState): Set[Argument] =
//    dState.p.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.body.contains(litArg.lit)).toSet[Argument]
}

