package aba.framework

import aba.{Argument, DisputeState}
import aba.fileParser.FileParser
import aba.reasoner.{Argument, DisputeState, LiteralArgument, RuleArgument}

import scala.annotation.tailrec

// companion object
object Framework {
  def apply(inputType: String, filePath: String): Framework = FileParser(inputType, filePath)
}

class Framework (val rules: Set[Rule],
                 val assumptions: Set[Literal],
                 val contraries: Set[Contrary],
                 val goals: Set[Literal],
                 val constraints: Set[Literal],
                 val alphabet: Map[String, Literal]
                ) {
  def initialDState: DisputeState = new DisputeState(goals.map(g => LiteralArgument(g)))

  // helpers
  def contrariesOf(literal: Literal): Set[Literal] = contraries.filter(_.assumption == literal).collect(_.contrary)

  def contrariesOf(literals: Set[Literal]): Set[Literal] = contraries.filter(ctr => literals.contains(ctr.assumption)).collect(_.contrary)

  def bRuleArgs(implicit dState: DisputeState): Set[RuleArgument] = dState.b.collect { case ruleArg: RuleArgument => ruleArg }

  def bLitArgs(implicit dState: DisputeState): Set[LiteralArgument] = dState.b.collect { case litArg: LiteralArgument => litArg }

  def pRuleArgs(implicit dState: DisputeState): Set[RuleArgument] = dState.p.collect { case ruleArg: RuleArgument => ruleArg }

  def pLitArgs(implicit dState: DisputeState): Set[LiteralArgument] = dState.p.collect { case litArg: LiteralArgument => litArg }


  // Paper specific

  // TODO: make dStates implicit?
  def defences(implicit dState: DisputeState): Set[Literal] = {
    pLitArgs.map(_.lit).intersect(assumptions)
  }

  def culprits(implicit dState: DisputeState): Set[Literal] = {
    contraries.filter(ctr => pLitArgs.map(_.lit).contains(ctr.contrary)).collect(_.assumption)
  }

  def remainingRulesP(implicit dState: DisputeState): Set[Rule] = {
    rules -- pRuleArgs.map(_.rule)
  }

  def remainingRulesB(implicit dState: DisputeState): Set[Rule] = {
    rules -- bRuleArgs.map(_.rule)
  }

  def blockedRulesB(implicit dState: DisputeState): Set[Rule] = {
    remainingRulesB.filter(_.body.intersect(culprits).nonEmpty)
  }

  def blockedRulesP(implicit dState: DisputeState): Set[Rule] = {
    remainingRulesP.filter( rule => (rule.body + rule.head)
      .intersect(culprits ++ contrariesOf(rule.body) ++ constraints ++ contrariesOf(defences)).nonEmpty
    )
  }

  def blockedAssumptionsP(implicit dState: DisputeState): Set[Literal] = {
    assumptions.filter(ass => (contrariesOf(ass) ++ culprits ++ contrariesOf(defences) ++ constraints).contains(ass))
  }

  // TODO: DRY
  def completePiecesP(implicit dState: DisputeState): Set[Argument] = {

    @tailrec
    def completePiecesRecP(args: Set[Argument])(implicit dState: DisputeState, nonAssumptionsLiterals: Set[Literal]): Set[Argument] = {

      val completeLitArgs = args.collect { case litArg: LiteralArgument => litArg }
      // val completeRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }

      val newRuleArgs = pRuleArgs.filter(arg => arg.rule.body.subsetOf(completeLitArgs.map(_.lit)))
      val newLitArgs = pLitArgs.filter(arg => nonAssumptionsLiterals.contains(arg.lit) && arg.pParents.intersect(args).nonEmpty)

      if (newRuleArgs.isEmpty && newLitArgs.isEmpty) args // do this as long as new arguments are created
      else completePiecesRecP(args ++ newRuleArgs ++ newLitArgs)
    }


    implicit val nonAssumptionsLiterals: Set[Literal] = alphabet.values.toSet -- assumptions

    val assumptionsArgs = pLitArgs.filter(arg => assumptions.contains(arg.lit)).toSet[Argument]

    completePiecesRecP(assumptionsArgs)
  }

  def completePiecesB(implicit dState: DisputeState): Set[Argument] = {

    @tailrec
    def completePiecesRecB(args: Set[Argument])(implicit dState: DisputeState, nonAssumptionsLiterals: Set[Literal]): Set[Argument] = {

      val completeLitArgs = args.collect { case litArg: LiteralArgument => litArg }
      // val completeRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }

      val newRuleArgs = bRuleArgs.filter(arg => arg.rule.body.subsetOf(completeLitArgs.map(_.lit)))
      val newLitArgs = bLitArgs.filter(arg => nonAssumptionsLiterals.contains(arg.lit) && arg.bParents.intersect(args).nonEmpty)

      if (newRuleArgs.isEmpty && newLitArgs.isEmpty) args // do this as long as new arguments are created
      else completePiecesRecB(args ++ newRuleArgs ++ newLitArgs)
    }


    implicit val nonAssumptionsLiterals: Set[Literal] = alphabet.values.toSet -- assumptions

    val assumptionsArgs = bLitArgs.filter(arg => assumptions.contains(arg.lit)).toSet[Argument]

    completePiecesRecB(assumptionsArgs)
  }

  def unexpandedPStatements(implicit dState: DisputeState): Set[Literal] = {
    pLitArgs.map(_.lit).diff(pRuleArgs.map(_.rule.head)) // diff = filterNot + contains
  }

  def fullyExpandedStatements(implicit dState: DisputeState): Set[Literal] = {
    bLitArgs.map(_.lit).diff(remainingRulesP.diff(blockedRulesB).map(_.head))
  }

  def playedBlockedPieces(implicit dState: DisputeState): Set[Argument] = {

    // TODO: what is the naming convention for wrapped recursive function?
    @tailrec
    def playedBlockedPiecesRec(args: Set[Argument])(implicit dState: DisputeState, fullyExpandedStatementsLitArgs: Set[LiteralArgument]): Set[Argument] = {

      val playedBlockedRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }
      val playedBlockedLitArgs = args.collect { case litArg: LiteralArgument => litArg }

      //val newLitArgs = fullyExpandedStatements.diff() bLitArgs.filter(arg => nonAssumptionsLiterals.contains(arg.lit) && arg.bParents.intersect(args).nonEmpty)
      val newLitArgs = fullyExpandedStatementsLitArgs.filterNot(arg => (bRuleArgs -- playedBlockedRuleArgs).map(_.rule.head).contains(arg.lit))
      val newRuleArgs = bRuleArgs.filter(arg => arg.rule.body.intersect(playedBlockedLitArgs.map(_.lit)).nonEmpty)

      if (newRuleArgs.isEmpty && newLitArgs.isEmpty) args // do this as long as new arguments are created
      else playedBlockedPiecesRec(args ++ newRuleArgs ++ newLitArgs)
    }


    implicit val fullyExpandedStatementsLitArgs: Set[LiteralArgument] = bLitArgs.filter(arg => fullyExpandedStatements.contains(arg.lit))

    val culpritArgs = bLitArgs.filter(arg => culprits.contains(arg.lit)).toSet[Argument]

    playedBlockedPiecesRec(culpritArgs)
  }

  def culpritsCandidates(implicit dState: DisputeState): Set[Literal] = (assumptions intersect bLitArgs.map(_.lit)) -- (defences ++ culprits)
}