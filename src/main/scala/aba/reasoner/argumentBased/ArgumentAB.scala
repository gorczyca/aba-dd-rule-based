package aba.reasoner.argumentBased

import aba.framework.{Framework, Literal, Rule}

// TODO: remove

// Argument (argument based approach)
abstract class ArgumentAB extends Product with Serializable {
  def consequence: Literal

  def premises: Set[Literal]

  //
  def topSubArguments: Set[ArgumentAB]

  def subArguments: Set[ArgumentAB]

  //
  def statements: Set[Literal]

  def assumptions(implicit framework: Framework): Set[Literal]

  def backwardExpand(complexArg: ComplexArgumentAB): ArgumentAB
}

case class SimpleArgumentAB(literal: Literal) extends ArgumentAB {

  override def toString: String = literal.id

  override def equals(obj: Any): Boolean = {
    obj match {
      case SimpleArgumentAB(lit) => lit.equals(literal)
      case _ => false
    }
  }

  override def hashCode(): Int = literal.hashCode()

  override def consequence: Literal = literal

  override def premises: Set[Literal] = Set(literal)

  //
  override def topSubArguments: Set[ArgumentAB] = Set(this)

  override def subArguments: Set[ArgumentAB] = Set(this)

  override def statements: Set[Literal] = Set(literal)

  def assumptions(implicit framework: Framework): Set[Literal] = framework.assumptions intersect Set(literal)

  override def backwardExpand(complexArg: ComplexArgumentAB): ArgumentAB = if (literal == complexArg.head) complexArg else this
}

case class ComplexArgumentAB(head: Literal, body: Set[ArgumentAB], rule: Rule) extends ArgumentAB {

  override def backwardExpand(complexArg: ComplexArgumentAB): ArgumentAB = {
    val newBody = body.map {
      case bodySimpleArg: SimpleArgumentAB => if (bodySimpleArg.literal == complexArg.head) complexArg else bodySimpleArg
      case bodyComplexArg: ComplexArgumentAB => bodyComplexArg.backwardExpand(complexArg)
    }

    ComplexArgumentAB(head, newBody, rule)
  }

  override def toString: String =
    head.toString + "<-" +
      body.toSeq.sortBy(_.consequence.id).map{
        case sim: SimpleArgumentAB => sim.toString // toString should be redundant
        case com: ComplexArgumentAB => "[".concat(com.toString).concat("]")
      }.mkString(",")

  override def hashCode(): Int = toString.hashCode

  override def equals(obj: Any): Boolean = obj match {
    // TODO:
    //case complexArg: ComplexArgumentAB => complexArg.hashCode == hashCode
    case complexArg: ComplexArgumentAB => (complexArg.head == head) && (complexArg.rule == rule) && complexArg.body.equals(body)
    case _ => false
  }

  def rules: Set[Rule] = body.collect { case complexArg: ComplexArgumentAB => complexArg }.flatMap(_.rules) + rule

  override def consequence: Literal = head

  override def premises: Set[Literal] = body.flatMap(_.premises)
  //

  override def topSubArguments: Set[ArgumentAB] =
    // product of set of sets, creating all possible bodies
    body.map(_.topSubArguments).foldLeft(Set(Set.empty[ArgumentAB])) { (x,y) => for (a <- x; b<-y) yield a + b }
      // map to ComplexArguments and add the head argument
      .map(bodyArgs => ComplexArgumentAB(head, bodyArgs, rule)) ++ Set(SimpleArgumentAB(head))

  override def subArguments: Set[ArgumentAB] = body.flatMap(_.subArguments) ++ topSubArguments

  override def statements: Set[Literal] = body.flatMap(_.statements) + head

  override def assumptions(implicit framework: Framework): Set[Literal] = body.flatMap(_.assumptions) ++ framework.assumptions.intersect(Set(head))
}
