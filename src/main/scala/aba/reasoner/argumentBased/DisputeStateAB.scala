package aba.reasoner.argumentBased

import aba.framework.{Framework, Rule}
import aba.move.Move.MoveType
import aba.reasoner.{Argument, DisputeState, LiteralArgument, RuleArgument}

import scala.annotation.tailrec


object DisputeStateAB_ {
  def apply(derivation: List[DisputeState])(implicit framework: Framework): DisputeStateAB = {

    implicit val lastState: DisputeState = derivation.last

    // helper
    def completeArgument(arg: ArgumentAB): Boolean = arg.premises subsetOf framework.assumptions

    @tailrec
    def convertPropRec(ruleArgs: Set[ComplexArgumentAB], allPArgs: Set[ArgumentAB]): Set[ArgumentAB] = {

      // extend backwards: PB1
      def maximalIncompleteForGoalsAndCulprits(arg: ArgumentAB): Boolean =
          (!completeArgument(arg)  // incomplete prop arg
           && !(allPArgs - arg).exists(otherArg => (otherArg.consequence == arg.consequence && completeArgument(otherArg)) || otherArg.subArguments.contains(arg)) // maximal
           && (framework.goals ++ framework.contrariesOf(framework.culprits)).contains(arg.consequence))

      val newBackwardExtended = ruleArgs.flatMap(ruleArg =>
        allPArgs.filter(maximalIncompleteForGoalsAndCulprits).map(_.backwardExpand(ruleArg))) -- allPArgs

      // extend forwards: PF1
      val newForwardExtended = ruleArgs.map(_.rule).flatMap(rule =>
        allPArgs.filter(arg => rule.body.contains(arg.consequence)).filter(completeArgument).subsets(rule.body.size)
          .filter(_.map(_.consequence) == rule.body).map(bodyArgs => ComplexArgumentAB(rule.head, bodyArgs, rule))).toSet[ArgumentAB] -- allPArgs // TODO:

      if (newBackwardExtended.isEmpty && newForwardExtended.isEmpty) allPArgs
      else convertPropRec(ruleArgs, allPArgs ++ newBackwardExtended ++ newForwardExtended)

    }

    @tailrec
    def convertOppRec(ruleArgs: Set[ComplexArgumentAB], allArgs: Set[ArgumentAB]): Set[ArgumentAB] = {

      def opposingArgument(arg: ArgumentAB): Boolean = framework.contrariesOf(framework.defences).contains(arg.consequence)
      def blockedArgument(arg: ArgumentAB): Boolean = framework.culprits.intersect(arg.assumptions).nonEmpty

      val blockedArguments = allArgs.filter(blockedArgument)

      val newBackwardExtended = ruleArgs.flatMap(ruleArg =>
        (allArgs.filter(opposingArgument) -- blockedArguments).map(_.backwardExpand(ruleArg))) -- allArgs

      val newForwardExtended = ruleArgs.map(_.rule).flatMap(rule =>
        (allArgs.filter(completeArgument) -- blockedArguments).filter(arg => rule.body.contains(arg.consequence)).subsets(rule.body.size)
          .filter(_.map(_.consequence) == rule.body).map(bodyArgs => ComplexArgumentAB(rule.head, bodyArgs, rule))).toSet[ArgumentAB] -- allArgs

      if (newBackwardExtended.isEmpty && newForwardExtended.isEmpty) allArgs
      else convertOppRec(ruleArgs, allArgs ++ newBackwardExtended ++ newForwardExtended)

    }

    // Case 1.
    // take only used assumptions and rules
    val (bArgs, pArgs) = derivation.drop(1).foldLeft( (Set.empty[Argument], Set.empty[Argument]) )( (argsTuple2, derState) =>
      (derState.move, derState.argument) match {
        case (Some(move), Some(argument)) =>
          if (move.isProponentMove) (argsTuple2._1 + argument, argsTuple2._2 + argument)
          else (argsTuple2._1 + argument, argsTuple2._2)
      }
    )

    // Case 2
    // take all arguments from b / p
//    val bArgs = lastState.b
//    val pArgs = lastState.p


    val bSimpleArgs = bArgs.collect { case litArg: LiteralArgument => litArg }.map(litArg => SimpleArgumentAB(litArg.lit))
    val bRuleArgs = bArgs.collect { case ruleArg: RuleArgument => ruleArg }.map(_.rule).map(rule => {
      //val body = bSimpleArgs.filter(arg => rule.body.contains(arg.literal)).toSet[ArgumentAB]
      ComplexArgumentAB(rule.head, rule.body.map(SimpleArgumentAB), rule)
    })

    val pSimpleArgs = bSimpleArgs.filter(simpleArg => pArgs.collect { case litArg: LiteralArgument => litArg }.map(_.lit).contains(simpleArg.literal))
    val pRuleArgs = bRuleArgs.filter(ruleArg=> pArgs.collect { case ruleArg: RuleArgument => ruleArg }.map(_.rule).contains(ruleArg.rule))

    val pArguments = convertPropRec(pRuleArgs, pRuleArgs ++ pSimpleArgs)
    val bArguments = convertOppRec(bRuleArgs, bRuleArgs ++ bSimpleArgs)

    DisputeStateAB(
      lastState.id,
      lastState.move,
      bArguments,
      pArguments)
  }
}


//object DisputeStateAB {
//  def apply(derivation: List[DisputeState])(implicit framework: Framework): DisputeStateAB = {
//
//    @tailrec
//    def obtainPRec(): Set[ArgumentAB]
//
//  }
//}


/**
 *
 * @param id
 * @param move
 * @param b - only relevant arguments
 * @param p - ditto
 */
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
