//package aba.reasoner


//abstract class Argument extends Product with Serializable { // To avoid mapping toSet[Argument]
//  def parents(implicit dState: DisputeState): Set[Argument]
//  def pParents(implicit dState: DisputeState): Set[Argument] = parents intersect dState.p
//  //def children(implicit dState: DisputeState): Set[Argument] = dState.b.filter(_.parents.contains(this))
//  def children(implicit dState: DisputeState): Set[Argument]
//  def pChildren(implicit dState: DisputeState): Set[Argument] = children intersect dState.p
//
//  def uid: String = if (hashCode() < 0) "an" + Math.abs(hashCode()).toString else "a" + hashCode().toString
//}
//
//case class LiteralArgument(lit: Literal) extends Argument {
//
//  // for preventing from having two same arguments (but different objects) in a set
//  override def equals(obj: Any): Boolean = {
//    obj match {
//      case LiteralArgument(literal) => literal.equals(lit)
//      case _ => false
//    }
//  }
//
//  // same as above
//  override def hashCode(): Int = lit.hashCode()
//
//  override def toString: String = lit.toString
//
//  override def parents(implicit dState: DisputeState): Set[Argument] =
//    dState.b.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.head == this.lit).toSet[Argument]
//
////  override def pParents(implicit dState: DisputeState): Set[Argument] =
////    dState.p.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.head == this.lit).toSet[Argument]
//  override def children(implicit dState: DisputeState): Set[Argument] =
//    dState.b.collect { case ruleArg: RuleArgument => ruleArg }.filter(_.rule.body.contains(this.lit)).toSet[Argument]
//}
//
//case class RuleArgument(rule: Rule) extends Argument {
//
//  //
//  override def equals(obj: Any): Boolean = {
//    obj match {
//      case ruleArg: RuleArgument => ruleArg.rule.eq(rule)
//      case _ => false
//    }
//  }
//
//  //
//  override def hashCode(): Int = rule.hashCode()
//
//  override def parents(implicit dState: DisputeState): Set[Argument] =
//    dState.b.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.body.contains(litArg.lit)).toSet[Argument]
//
//  // toString called implicitly
//  override def toString: String = rule.toString
//
////  override def pParents(implicit dState: DisputeState): Set[Argument] =
////    dState.p.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.body.contains(litArg.lit)).toSet[Argument]
//  override def children(implicit dState: DisputeState): Set[Argument] =
//    dState.b.collect { case litArg: LiteralArgument => litArg }.filter(litArg => this.rule.head == litArg.lit).toSet[Argument]
//
//  def parentsIncludingCulprits(implicit dState: DisputeState, framework: Framework): Set[Argument] = {
//    this.parents ++ framework.culprits.intersect(this.rule.body).map(LiteralArgument)
//  }
//}
