package aba.reasoner.argumentBased2
import aba.framework.{Framework, Literal, Rule}

import scala.annotation.tailrec





object DisputeStateAB2 {

  def create_arguments(goals: Set[Literal], rules: Set[Rule])(implicit framework: Framework): Set[ArgumentTree] = {

    @tailrec
    def create_args_rec(stack: List[ArgumentTree], completedArguments: Set[ArgumentTree]): Set[ArgumentTree] = {
      // add to "completed" the actual complete arguments (without any endpoints OR those that have endpoints that we don't have rules for)
      val (actualCompleteArguments, uncompleteArguments) = stack.partition(_.isComplete)
      val (circularArguments, uncircularArguments) = uncompleteArguments.partition(_.isCircular)
      val (furtherUnexpandable, furtherExpandable) = uncircularArguments.partition(_.endpoints.forall{ case (argNode, endpointRules) => !rules.exists(_.head == argNode.data) })

      val currentStack = furtherExpandable
      val newComplete = completedArguments ++ actualCompleteArguments ++ circularArguments ++ furtherUnexpandable

      if (currentStack.isEmpty)
        newComplete
      else {

        val poppedArg::stackAfter = currentStack
        val poppedArgEndpointLit = poppedArg.endpoints.filter(endpoint => rules.exists(_.head == endpoint._1.data)).head._1.data
        val rulesToUse = rules.filter(_.head == poppedArgEndpointLit)

        // val poppedArgEndpoints = poppedArg.endpoints.filter(_._1.data == poppedArgEndpointLit)
        // TODO: mark situation when has no children but rule has no body!!! JUST some token meaning that its empty
        val extendedArgs = rulesToUse.map(rule => poppedArg.dCopy(rule))
        //val uncomplete = extendedArgs -- complete

        val newStack = stackAfter ++ extendedArgs

        create_args_rec(newStack, newComplete)
      }
    }

    val initialStack = goals.map(lit => ArgumentTree(ArgumentNode(lit))).toList

    create_args_rec(initialStack, Set.empty[ArgumentTree])
  }


}
