package aba.reasoner.argumentBased2

import aba.framework.{Framework, Literal, Rule}

import java.util.UUID
import scala.annotation.tailrec


object ArgumentNode {
  def newUuid: String = UUID.randomUUID().toString.replace('-', '_')

  def apply(): ArgumentNode = {
    new ArgumentNode(Literal("FACT"), Set.empty[ArgumentNode], factNode=true, uuid = newUuid)
  }

  def apply(lit: Literal): ArgumentNode = {
    new ArgumentNode(lit, Set.empty[ArgumentNode], uuid = newUuid)
  }

  def apply(lit: Literal, children: Set[ArgumentNode]): ArgumentNode = {
    new ArgumentNode(lit, children, uuid = newUuid)
  }

  def apply(copyFrom: ArgumentNode, children: Set[ArgumentNode]): ArgumentNode = {
    new ArgumentNode(copyFrom.data, children, factNode = copyFrom.factNode, uuid = newUuid)
  }

}


class ArgumentNode(val data: Literal,
                        val children: Set[ArgumentNode],
                        val factNode: Boolean = false,
                        val uuid: String) {

  // TODO: this better, with the UUIDs



  override def toString: String = if (children.isEmpty) s"$data" else s"$data ← [${children.map(_.toString).mkString(",")}]"

  def uid: String = s"arg_node_${data}_$uuid"

  def flattenTree: Seq[ArgumentNode] = {
   children.map(_.flattenTree).toSeq.flatten :+ this
  }

  def flattenTreeSet: Set[ArgumentNode] = {
    children.flatMap(_.flattenTree) + this
  }


  def deepCopy1(childrenMap: Map[ArgumentNode, Set[ArgumentNode]], currentEndpoints: Map[ArgumentNode, Set[Rule]]): (ArgumentNode, Map[ArgumentNode, Set[Rule]]) = {  // TODO: if this works, should be renamed from enpointsMap to childrenMap

    if (this.children.isEmpty && childrenMap.contains(this)) {
      // this should be for the root only
      val children = childrenMap(this)
      val newArgNode = ArgumentNode(this, children)

      if (currentEndpoints.contains(this)) {
        val endpointsRules = currentEndpoints(this)
        val newEndpoints = currentEndpoints - this + (newArgNode -> endpointsRules)
        (newArgNode, newEndpoints)
      } else {
        (newArgNode, currentEndpoints)
      }
      //ArgumentNode(this.data, children)
    } else if (this.children.isEmpty) {

      val newArgNode = ArgumentNode(this, children)

      if (currentEndpoints.contains(this)) {
        val endpointsRules = currentEndpoints(this)
        val newEndpoints = currentEndpoints - this + (newArgNode -> endpointsRules)
        (newArgNode, newEndpoints)
      } else {
        (newArgNode, currentEndpoints)
      }
      //ArgumentNode(this.data)
    } else {
      // TODO: zmienic na childrenEndpointsSet
      val endpointsSet = childrenMap.keySet


      val nonEndpointsChildren = this.children -- endpointsSet
      val endpointsChildren = this.children.intersect(endpointsSet).map(argNode => ArgumentNode(argNode.data, childrenMap(argNode))) //argNode => ArgumentNode(argNode.data, endpointsMap(argNode)._2, None)) // should be exactly one always
//      val newChildren = nonEndpointsChildren.map(_.deepCopy1(childrenMap)) union endpointsChildren
      if (nonEndpointsChildren.nonEmpty) {
        val (newChildren, newEndpoints) = nonEndpointsChildren.map(_.deepCopy1(childrenMap, currentEndpoints)).unzip
        (ArgumentNode(this.data, newChildren union endpointsChildren), newEndpoints.flatten.toMap)
      } else {
        (ArgumentNode(this.data, endpointsChildren), currentEndpoints)
      }


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
        (argumentNode, circularities, true) // todo: notify that circular
        // TODO: tutaj jest cos nie tak
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

          // TODO: tu zapewne brakuje case'a ze regula ma puste body

          val ruleToUse = rulesUsedGlobally.filter(_.head == endpointLit).head
          val relevantEndpoints = endpoints.filter(_._1.data == endpointLit)
          // TODO: to zmienilem
          val remainingEndpoints = endpoints -- relevantEndpoints.keySet

          // TODO: wazne!!!
          // TODO: co z remaining endpoints??
          if (ruleToUse.body.nonEmpty) {

            // currentEndpoint -> (rulesUsed, newEndpointsSet)
            val endpointsNewRulesUsedNewEndpointsMap = relevantEndpoints.map { case (argNode, rules) => argNode -> (rules + ruleToUse, ruleToUse.body.map(lit => ArgumentNode(lit))) }
            // filter out assumptions, so that they are not endpoints
            val nonAssumptionsEndpointsMap = endpointsNewRulesUsedNewEndpointsMap.map { case (argNode, (rules, endpoints)) => argNode -> (rules, endpoints.filter(node => !framework.assumptions.contains(node.data))) }
            // remove entries with no endpoints
            val flattenedNonAssumptionsEndpointsMap = nonAssumptionsEndpointsMap.filter { case (_, (_, endpoints)) => endpoints.nonEmpty }

            // new endpoints. Each of type endpoint: rulesUsed
            val newEndpoints = flattenedNonAssumptionsEndpointsMap.values.map { case (rules, argNodes) => argNodes.map(argNode => argNode -> rules) }.flatten.toMap // more readable when not flatMap

            // TODO: to tez dodalem
            val actualNewEndpoints = (remainingEndpoints.toSeq ++ newEndpoints.toSeq).groupBy(_._1).map { case (arguNode, seq) => arguNode -> seq.map(_._2).flatten.toSet }.toMap


            // all new children. Map endpoint: new children of endpoint to create a new argument
            // when creating new argument, copy everything and when encountered argNode replace it with new Argument with the same data but with children
            val newChildrenMap = endpointsNewRulesUsedNewEndpointsMap.map { case (argNode, set) => (argNode, set._2) }

            val (newArgNode, newEndpoints2) = argumentNode.deepCopy1(newChildrenMap, actualNewEndpoints) // was newEndpoints
            furtherExtend(newArgNode, newEndpoints2)
          } else {
            val childrenMap = relevantEndpoints.map { case (argNode, _) => argNode -> Set(ArgumentNode()) }
            val (newArgNode, newEndpoints) = argumentNode.deepCopy1(childrenMap, remainingEndpoints)
            furtherExtend(newArgNode, newEndpoints)
          }
        }

      }

    }

    furtherExtend(this, Map(this -> rulesInCurrentBranch))


  }
}
object ArgumentTree {
  def apply(argumentNode: ArgumentNode): ArgumentTree = {
    new ArgumentTree(argumentNode, Map(argumentNode -> Set.empty[Rule]), Set.empty[Rule], uuid = newUuid)
  }

  def apply(root: ArgumentNode,
            endpoints: Map[ArgumentNode, Set[Rule]],
            rules: Set[Rule],
            isComplete: Boolean,
            isCircular: Boolean,
            circularArgs: Option[Set[ArgumentNode]]): ArgumentTree = {
    new ArgumentTree(root, endpoints, rules, isComplete=isComplete, isCircular=isCircular, circularArgs=circularArgs, uuid=newUuid)
  }

  def newUuid: String = UUID.randomUUID().toString.replace('-', '_')

}


class ArgumentTree(val root: ArgumentNode,
                   val endpoints: Map[ArgumentNode, Set[Rule]], // pointer to siblings where its located
                   val rulesUsed: Set[Rule],
                   val isComplete: Boolean = false,
                   val isCircular: Boolean = false,
                   val circularArgs: Option[Set[ArgumentNode]] = None,
                   val uuid: String) {



//  override def toString: String = s"${root.toString}\nENDPOINTS:\n${endpoints.map{ case (argNode, rules) => s"${argNode.data}: {${rules.mkString(",")}}" }}\nuid"
  override def toString: String = s"${root.toString}\nENDPOINTS:\n${endpoints.map{ case (argNode, rules) => s"${argNode.data}: {${rules.mkString(",")}}" }}"

//  def uid: String = s"argument_${root.data}_${ if (hashCode() < 0) "an" + Math.abs(hashCode()).toString else "a" + hashCode().toString }"
  def uid: String = s"argument_${root.data}_$uuid"


  def dCopy(rule: Rule)(implicit framework: Framework): ArgumentTree = {
    val relevantEndpoints = this.endpoints.filter(_._1.data == rule.head) // take all endpoints with head as current rule
    val rulesUsedGlobally = rulesUsed + rule // expand set of ALL rules
    val remainingEndpoints = this.endpoints -- relevantEndpoints.keySet

    if (rule.body.nonEmpty) {
      // if there is body

      // currentEndpoint -> (rulesUsed, newEndpointsSet)
      // TODO: tutaj zanim dodam, sprawdzic czy nie bedzie wtedy circular
      val circularEndpoints = relevantEndpoints.filter{ case (argNode, rules) => rules.exists(_.head == rule.head) }
      // TODO: circularity does not work
      // !!!


      val endpointsNewRulesUsedNewEndpointsMap = relevantEndpoints.map { case (argNode, rules) => argNode -> (rules + rule, rule.body.map(lit => ArgumentNode(lit))) }

      // currendEndpoint -> newChildren that are assumptions
      val assumptionChildrenNodes = endpointsNewRulesUsedNewEndpointsMap.map { case (argNode, (_, endpoints)) => argNode -> endpoints.filter(node => framework.assumptions.contains(node.data)) }
      // TODO: filter out empty for clarity

      // currentEndpoint -> (rulesUsed, newEndpointsWithoutAssumptions)
      val nonAssumptionsEndpointsMap = endpointsNewRulesUsedNewEndpointsMap.map { case (argNode, (rules, endpoints)) => argNode -> (rules, endpoints.filter(node => !framework.assumptions.contains(node.data))) }
      // remove those that have ONLY assumptions - leave TRUE endpoints
      val flattenedNonAssumptionsEndpointsMap = nonAssumptionsEndpointsMap.filter { case (_, (_, endpoints)) => endpoints.nonEmpty }

      // map:
      // currentEndpoint -> set[tuple(new child, its endpoints, is it circular?)]
      val endpointsNewChildrenNewEndpointsIsCircularMap = flattenedNonAssumptionsEndpointsMap.map { case (argNode, (rules, nextEndpointsNodes)) => (argNode, nextEndpointsNodes.map(_.extend(rules, rulesUsedGlobally))) }

      // map:
      // currentEndpoint -> set[tuple(newChild, its endpoints)]
      val endpointsNewChildrenNewEndpointsMap = endpointsNewChildrenNewEndpointsIsCircularMap.map { case (argNode, tupleSet) => (argNode, tupleSet.map { case (arg, tupleMap, _) => (arg, tupleMap) }) }

      // for circularity
      // TODO: just an simple solution, but should be corrected - take only those that have no children (are leaves)
      val circularArgs = endpointsNewChildrenNewEndpointsIsCircularMap.values.flatten.filter { case (arg, tupleMap, isCircular) => isCircular }.map(_._1).filter(_.children.isEmpty)
      val isCircular = circularArgs.nonEmpty
      val circularArgsOption = if (isCircular) Some(circularArgs.toSet) else None

      // for new children
      // map
      // currentEndpoint -> its children
      val childrenMap = endpointsNewChildrenNewEndpointsIsCircularMap.map { case (argNode, set) => (argNode, set.map(_._1)) }
      val childrenWithAssumptionsMap = (childrenMap.toSeq ++ assumptionChildrenNodes.toSeq).groupBy(_._1).map { case (arguNode, seq) => arguNode -> seq.map(_._2).flatten.toSet }.toMap

      // new endpoints
      val newlyCreatedEndpoints = endpointsNewChildrenNewEndpointsMap.values.flatten.map { case (_, actualEndpointsMap) => actualEndpointsMap }.flatten.toMap
      // new endpoints together with remaining endpoints
      val actualNewEndpoints = (remainingEndpoints.toSeq ++ newlyCreatedEndpoints.toSeq).groupBy(_._1).map { case (arguNode, seq) => arguNode -> seq.map(_._2).flatten.toSet }.toMap
      val isComplete = actualNewEndpoints.isEmpty // TODO: to niech sie po prostu wylicza

      val (newRoot,actualNewEndpoints2) = root.deepCopy1(childrenWithAssumptionsMap, actualNewEndpoints)

      ArgumentTree(newRoot, actualNewEndpoints2, rulesUsedGlobally, isCircular = isCircular, isComplete = isComplete,
        circularArgs = circularArgsOption)
    } else {
      // if empty body
      val childrenMap = relevantEndpoints.map { case (argNode, _) => argNode -> Set(ArgumentNode()) }
      val isComplete = remainingEndpoints.isEmpty

      val (newRoot, newEndpoints) = root.deepCopy1(childrenMap, remainingEndpoints)
      ArgumentTree(newRoot, newEndpoints, rulesUsedGlobally, isComplete = isComplete, isCircular=false, circularArgs=None)

    }

  }

}