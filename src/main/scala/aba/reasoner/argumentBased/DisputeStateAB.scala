package aba.reasoner.argumentBased

import aba.framework.{Framework, Literal, Rule}
import aba.move.Move.MoveType
import aba.reasoner.DisputeState

import scala.annotation.tailrec


object DisputeStateAB {
  def apply(dState: DisputeState): DisputeStateAB = {

    @tailrec
    def convertRec(arguments: Set[ComplexArgumentAB]): Set[ComplexArgumentAB] = {
    //def convertRec(arguments: Set[ComplexArgumentAB])(implicit rules: Set[Rule]): Set[ArgumentAB] = {

      // backward extend
      val newBackwardExtended = arguments.flatMap(arg => {
        arguments.map(_.backwardExpand(arg))
      }) -- arguments

      // TODO: it should be possible to only propagate backward and still obtain all arguments?
      // // forward extend
      //val newForwardExtended = rules.flatMap(rule => {
      //  val bodies = arguments.filter(complexArg => rule.body.contains(complexArg.head))
      //    .subsets(rule.body.size).filter(_.map(_.head) == rule.body)
      //
      //  bodies.map( body_ => ComplexArgumentAB(rule.head, body_, rule))
      //})

      if (newBackwardExtended.isEmpty) arguments
      else convertRec(arguments ++ newBackwardExtended)
    }

    val simpleArgs = dState.bLitArgs.map(litArg => SimpleArgumentAB(litArg.lit))
    //implicit val usedRules: Set[Rule] = dState.bRuleArgs.map(_.rule)
    val usedRules: Set[Rule] = dState.bRuleArgs.map(_.rule)
    val ruleArgs = usedRules.map(rule => {

      val head = simpleArgs.filter(_.literal == rule.head).head.literal // TODO: take exactly 1 element
      val body = simpleArgs.filter(arg => rule.body.contains(arg.literal)).toSet[ArgumentAB]

      ComplexArgumentAB(head, body, rule)
    })

    val complexArgs = convertRec(ruleArgs)

    val pSimpleArgs = simpleArgs.filter(arg => dState.pLitArgs.map(_.lit).contains(arg.literal))
    val pComplexArgs = complexArgs.filter(arg => arg.rules subsetOf dState.pRuleArgs.map(_.rule))

    DisputeStateAB(
      dState.id,
      dState.move,
      simpleArgs ++ complexArgs,
      pSimpleArgs ++ pComplexArgs)
  }
}

case class DisputeStateAB(id: Int,
                          move: Option[MoveType],
                          b: Set[ArgumentAB], // TODO: should I do p here too?
                          p: Set[ArgumentAB]
                         ) {

  def decorateArguments(implicit framework: Framework, dState: DisputeState): Seq[(ArgumentAB, String)] = {

    // TODO: return Booleans instead
    // helper
    def completeArgument(arg: ArgumentAB): Boolean = arg.premises subsetOf framework.assumptions

    def isProponentArgument(arg: ArgumentAB): String = if (p contains arg) "$" else ""
    def isOpposingArgument(arg: ArgumentAB): String = if (framework.contrariesOf(framework.defences).contains(arg.consequence)) "!" else ""

    def isCompleteArgument(arg: ArgumentAB): String = if(completeArgument(arg)) "*" else ""
    // TODO: check if sub / topArgs work
    def isMaximalIncompletePropForGoalsAndCulpritsContraries(arg: ArgumentAB): String = {

      if (!completeArgument(arg) && p.contains(arg) // incomplete prop arg
        && !(p - arg).exists(otherArg => (otherArg.consequence == arg.consequence && completeArgument(otherArg)) || otherArg.subArguments.contains(arg)) // maximal
        && (framework.goals ++ framework.contrariesOf(framework.culprits)).contains(arg.consequence)) "#" else ""
    }

    def isBlockedArgument(arg: ArgumentAB): String = if ((framework.culprits intersect arg.assumptions).nonEmpty) "--" else ""

    val decFunctions = isProponentArgument _ :: isOpposingArgument _ :: isCompleteArgument _ :: isMaximalIncompletePropForGoalsAndCulpritsContraries _ :: isBlockedArgument _ :: Nil

    b.toSeq.sortBy(_.consequence.id).map(arg =>
      (arg, decFunctions.foldLeft("")((currDec, func) => currDec + func(arg)) + arg)
    )
  }

  def decorateRules(implicit framework: Framework, dState: DisputeState): Seq[(Rule, String)] = {

    def isBlockedDueToCulpritsInBody(rule: Rule): String = if (rule.body.intersect(framework.culprits).nonEmpty) "--" else ""
    def isBlockedDueToConstraintsContrariesOrInconsistent(rule: Rule): String = if (framework.contrariesOf(rule.body + rule.head).intersect(rule.body  + rule.head).nonEmpty ||
      (rule.body + rule.head).intersect(framework.constraints ++ framework.contrariesOf(framework.defences)).nonEmpty) "~" else ""

    // TODO: ??? // URGENT
    def isBlockedDueToFlabbines(rule: Rule): String = if ((dState.pRuleArgs.map(_.rule) - rule).exists(_.head == rule.head)) "~" else ""

    val decFunctions = isBlockedDueToCulpritsInBody _ :: isBlockedDueToConstraintsContrariesOrInconsistent _ :: isBlockedDueToFlabbines _ :: Nil

    framework.rules.toSeq.sortBy(_.head.id).map(rule =>
      (rule, decFunctions.foldLeft("")((currDec, func) => currDec + func(rule)) + rule)
    )
  }
}
