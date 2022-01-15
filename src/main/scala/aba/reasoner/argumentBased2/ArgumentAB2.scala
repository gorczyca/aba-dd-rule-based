package aba.reasoner.argumentBased2

import aba.framework.{Framework, Literal, Rule}

import scala.annotation.tailrec


//import aba.framework.{Literal, Rule}
//
//abstract class ArgumentAB2 extends Product with Serializable { }
//
//case class SimpleArgumentAB2(literal: Literal) extends ArgumentAB2
//case class ComplexArgumentAB2(head: Literal, body: Set[ArgumentAB2], rule: Rule) extends ArgumentAB2

case class ArgumentNode(val data: Literal, val children: Set[ArgumentNode], val parent: Option[ArgumentNode]) {
  def this(literal: Literal) = {
    this(literal, Set.empty[ArgumentNode], None)
  }

  override def toString: String = if (children.isEmpty) s"$data" else s"$data ← [${children.map(_.toString).mkString(",")}]"



  //  def deepCopy(endpoints: Map[ArgumentNode, Set[Rule]], rule: Rule): (ArgumentNode, Map[ArgumentNode, Set[Rule]]) = {
  //    // only for the top one
  //
  //    val newArgumentEndpoints = endpoints.map{
  //      case (argNode, rules) => argNode -> (Set(rules.toSeq: _*) + rule, rule.body.map(lit => new ArgumentNode(lit)))} // copying the set of rules
  //
  //
  //
  //
  //    if (endpoints.contains(this)) {
  //      val newChidren = rule.body.map(lit => ArgumentNode(lit, Set.empty[ArgumentNode], None))
  //      val newEndpoints = newChidren.map((_, Set(rule))).toMap
  //      (ArgumentNode(data, newChidren, None), newEndpoints)
  //    } else if (endpoints.keySet.intersect(children).nonEmpty) {
  //      val childrenEndpoints = endpoints.keySet.intersect(children)
  //
  //    }
  //
  //
  //
  //    val childrenCopy = if (children.isEmpty) Set.empty[ArgumentNode]
  //      else children.map{ case argNode: ArgumentNode => ArgumentNode(argNode.data, argNode.children.map(_.deepCopy()), ) }
  //
  //
  //
  //
  //  }

  // TODO: previously I had
//  def deepCopy1(endpointsMap: Map[ArgumentNode, (Set[Rule], Set[ArgumentNode])]): ArgumentNode = {
  def deepCopy1(endpointsMap: Map[ArgumentNode, Set[ArgumentNode]]): ArgumentNode = {  // TODO: if this works, should be renamed from enpointsMap to childrenMap

    if (this.children.isEmpty && endpointsMap.contains(this)) {
      // this should be for the root only ???
      val children = endpointsMap(this)
      ArgumentNode(this.data, children, None)
    } else if (this.children.isEmpty) {
      ArgumentNode(this.data, Set.empty[ArgumentNode], None)
    } else {
      val endpointsSet = endpointsMap.keySet

      val nonEndpointsChildren = this.children -- endpointsSet
//      val endpointsChildren = this.children.intersect(endpointsSet).map(endpointsMap).flatten  //argNode => ArgumentNode(argNode.data, endpointsMap(argNode)._2, None)) // should be exactly one always
      val endpointsChildren = this.children.intersect(endpointsSet).map(argNode => ArgumentNode(argNode.data, endpointsMap(argNode), None)) //argNode => ArgumentNode(argNode.data, endpointsMap(argNode)._2, None)) // should be exactly one always

      val newChildren = nonEndpointsChildren.map(_.deepCopy1(endpointsMap)) union endpointsChildren

      ArgumentNode(this.data, newChildren, None)
    }
  }

// TODO: previously I had
//  def extend(currentEndpoints: Map[ArgumentNode, Set[Rule]], rulesUsedGlobally: Set[Rule])(implicit framework: Framework): (ArgumentNode, Map[ArgumentNode, Set[Rule]]) = {
  def extend(rulesInCurrentBranch: Set[Rule], rulesUsedGlobally: Set[Rule])(implicit framework: Framework): (ArgumentNode, Map[ArgumentNode, Set[Rule]], Boolean) = {
    // returns argTree, isCircular, isFinished

    @tailrec                                                                                            // last Boolean means if circularities
    def furtherExtend(argumentNode: ArgumentNode, endpoints: Map[ArgumentNode, Set[Rule]]): (ArgumentNode, Map[ArgumentNode, Set[Rule]], Boolean) = {
      // check if circular

      if (endpoints.isEmpty) {
        return (argumentNode, endpoints, false) // todo: notify that it is finished - but it is only one branch that is finished - not enough
      }

      val circularities = endpoints.filter { case (argNode, rules) => rules.exists(_.head == argNode.data) }
      if (circularities.nonEmpty) {
        (argumentNode, endpoints, true) // todo: notify that circular
      } else {
        // if no circularities
        // check if one can further extend using rules already used in this argument
        val expandableEndpoints = endpoints.keySet.filter(argNode => rulesUsedGlobally.exists(_.head == argNode.data))
        if (expandableEndpoints.isEmpty) {
          // cannot further expand, return what you have
          (argumentNode, endpoints, false)
        } else {
          // there can be many with many rules. Do it one by one
          val endpointLit = expandableEndpoints.head.data
          // there should be EXACTLY 1 rule
          val ruleToUse = rulesUsedGlobally.filter(_.head == endpointLit).head
//          val nonAssumptionsBody = ruleToUse.body -- framework.assumptions
          val relevantEndpoints = endpoints.filter(_._1.data == endpointLit)
          // adding rule to set shuld work fine, because should be defined by Rule
          val endpointsMap = relevantEndpoints.map { case (argNode, rules) => argNode -> (rules + ruleToUse, ruleToUse.body.map(lit => new ArgumentNode(lit))) }

          // val assumptionChildrenNodes = endpointsMap.map { case (argNode, (rules, endpoints)) => argNode -> endpoints.filter(node => framework.assumptions.contains(node.data)) }

          val nonAssumptionsEndpointsMap = endpointsMap.map { case (argNode, (rules, endpoints)) => argNode -> (rules, endpoints.filter(node => !framework.assumptions.contains(node.data)))  }
          val flattenedNonAssumptionsEndpointsMap = nonAssumptionsEndpointsMap.filter{ case (argNode, (rules, endpoints)) => endpoints.nonEmpty }



          val newEndpoints = flattenedNonAssumptionsEndpointsMap.values.map { case (rules, argNodes) => argNodes.map(argNode => argNode -> rules) }.flatten.toMap // more readable when not flatMap

          val childrenMap = endpointsMap.map{ case (argNode, set) => (argNode, set._2) }



          // TODO: previously i had
//          val newArgNode = argumentNode.deepCopy1(endpointsMap)
          val newArgNode = argumentNode.deepCopy1(childrenMap)
          furtherExtend(newArgNode, newEndpoints)
        }

      }

    }

    furtherExtend(this, Map(this -> rulesInCurrentBranch))


  }
}

case class ArgumentTree(val root: ArgumentNode,
                   val endpoints: Map[ArgumentNode, Set[Rule]], // pointer to siblings where its located
                   val rulesUsed: Set[Rule],
                   val isComplete: Boolean = false,
                   val isCircular: Boolean = false) {

  def this(argumentNode: ArgumentNode) = {
    this(argumentNode, Map(argumentNode -> Set.empty[Rule]), Set.empty[Rule])
  }

  override def toString: String = s"${root.toString}\nENDPOINTS:\n${endpoints.map{ case (argNode, rules) => s"${argNode.data}: {${rules.mkString(",")}}" }}"

  def dCopy(rule: Rule)(implicit framework: Framework): ArgumentTree = {
    val relevantEndpoints = this.endpoints.filter(_._1.data == rule.head)
    val rulesUsedGlobally = rulesUsed + rule
    // TODO: poprawic: trzeba utworzyc nowe nody dla assumptionow, ale nie moga byc one endpointami
    val nonAssumptionsRuleBody = rule.body -- framework.assumptions
    // TODO: this was here before
    val endpointsMap = relevantEndpoints.map { case (argNode, rules) => argNode -> (rules + rule, rule.body.map(lit => new ArgumentNode(lit)))  }
    // TODO: previously i had
    //val endpointsMap2 = endpointsMap.map { case (argNode, (rules, nextEndpointsNodes)) => (argNode, nextEndpointsNodes.map(_.extend(nextEndpointsNodes.map((_, rules)).toMap, rulesUsedGlobally))) }

    val assumptionChildrenNodes = endpointsMap.map { case (argNode, (rules, endpoints)) => argNode -> endpoints.filter(node => framework.assumptions.contains(node.data)) }

    val nonAssumptionsEndpointsMap = endpointsMap.map { case (argNode, (rules, endpoints)) => argNode -> (rules, endpoints.filter(node => !framework.assumptions.contains(node.data)))  }
    val flattenedNonAssumptionsEndpointsMap = nonAssumptionsEndpointsMap.filter{ case (argNode, (rules, endpoints)) => endpoints.nonEmpty }

    // TODO: before we had (annotation)
//    val endpointsMap2: Map[ArgumentNode, Set[(ArgumentNode, Map[ArgumentNode, Set[Rule]])]] = flattenedNonAssumptionsEndpointsMap.map { case (argNode, (rules, nextEndpointsNodes)) => (argNode, nextEndpointsNodes.map(_.extend(rules, rulesUsedGlobally))) }
    val endpointsMap2 = flattenedNonAssumptionsEndpointsMap.map { case (argNode, (rules, nextEndpointsNodes)) => (argNode, nextEndpointsNodes.map(_.extend(rules, rulesUsedGlobally))) }

    val endpointsMap3 = endpointsMap2.map { case (argNode, tupleSet) => (argNode, tupleSet.map{ case (arg, tupleMap, isCircular) => (arg, tupleMap)  })}

    // TODO: potem stad dokladnie wziac co jest circular
    val isCircular = endpointsMap2.values.flatten.exists{ case (arg, tupleMap, isCircular) => isCircular  }

    val childrenMap = endpointsMap2.map{ case (argNode, set) => (argNode, set.map(_._1)) }
    val childrenWithAssumptionsMap = (childrenMap.toSeq ++ assumptionChildrenNodes.toSeq).groupBy(_._1).map { case (arguNode, seq) => arguNode -> seq.map(_._2).flatten.toSet }.toMap

    val newEndpoints2 = endpointsMap3.values.flatten.map { case (_, actualEndpointsMap) => actualEndpointsMap }.flatten.toMap
    val isComplete = newEndpoints2.isEmpty

    val newEndpoints = endpointsMap.values.map { case (rules, argNodes) => argNodes.map(argNode => argNode -> rules) }.flatten.toMap // more readable when not flatMap


    val actualNewEndpoints = this.endpoints -- relevantEndpoints.keySet
//    val actualNewEndpoints2 = (actualNewEndpoints.toSeq ++ newEndpoints2.toSeq).toMap
    // TODO: previously we didnt merge maps very well
    val actualNewEndpoints2 = (actualNewEndpoints.toSeq ++ newEndpoints2.toSeq).groupBy(_._1).map { case (arguNode, seq) => arguNode -> seq.map(_._2).flatten.toSet }.toMap // TODO: dangerous if two of the same, but should be fine


// todo: previously i had
//    ArgumentTree(root.deepCopy1(endpointsMap), newEndpoints2, rulesUsedGlobally)
    ArgumentTree(root.deepCopy1(childrenWithAssumptionsMap), actualNewEndpoints2, rulesUsedGlobally, isCircular=isCircular, isComplete=isComplete)
  }

}