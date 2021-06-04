package aba.framework

import aba.fileParser.FileParser
import aba.move.Move.MoveType
import aba.reasoner.{Argument, DisputeState, LiteralArgument, PotentialMove, RuleArgument}

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
  def initialDState: DisputeState = DisputeState(goals)

  // helpers
  def contrariesOf(literal: Literal): Set[Literal] = contraries.filter(_.assumption == literal).collect(_.contrary)

  def contrariesOf(literals: Set[Literal]): Set[Literal] = contraries.filter(ctr => literals.contains(ctr.assumption)).collect(_.contrary)

  def bRuleArgs(implicit dState: DisputeState): Set[RuleArgument] = dState.bRuleArgs

  def bLitArgs(implicit dState: DisputeState): Set[LiteralArgument] = dState.bLitArgs

  def pRuleArgs(implicit dState: DisputeState): Set[RuleArgument] = dState.pRuleArgs

  def pLitArgs(implicit dState: DisputeState): Set[LiteralArgument] = dState.pLitArgs


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

  def remainingNonBlockedPRules(implicit dState: DisputeState): Set[Rule] = remainingRulesP -- blockedRulesP

  def remainingNonBlockedBRules(implicit dState: DisputeState): Set[Rule] = remainingRulesB -- blockedRulesB

  def blockedAssumptionsP(implicit dState: DisputeState): Set[Literal] = {
    assumptions.filter(ass => (contrariesOf(ass) ++ culprits ++ contrariesOf(defences) ++ constraints).contains(ass))
  }

  // TODO: DRY
  def completePiecesP(implicit dState: DisputeState): Set[Argument] = {

    @tailrec
    def completePiecesRecP(args: Set[Argument])(implicit dState: DisputeState, nonAssumptionsLiterals: Set[Literal]): Set[Argument] = {

      val completeLitArgs = args.collect { case litArg: LiteralArgument => litArg }
      val completeRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }

      val newRuleArgs = pRuleArgs.filter(arg => arg.rule.body.subsetOf(completeLitArgs.map(_.lit))) -- completeRuleArgs // remove old args, consider new rule args only
      val newLitArgs = pLitArgs.filter(arg => nonAssumptionsLiterals.contains(arg.lit) && arg.pParents.intersect(args).nonEmpty)  -- completeLitArgs // same


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
      val completeRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }

      val newRuleArgs = bRuleArgs.filter(arg => arg.rule.body.subsetOf(completeLitArgs.map(_.lit))) -- completeRuleArgs  // remove arguments that were already obtained before
      val newLitArgs = bLitArgs.filter(arg => nonAssumptionsLiterals.contains(arg.lit) && arg.parents.intersect(args).nonEmpty) -- completeLitArgs //

      if (newRuleArgs.isEmpty && newLitArgs.isEmpty) args // do this as long as new arguments are created
      else completePiecesRecB(args ++ newRuleArgs ++ newLitArgs)
    }


    implicit val nonAssumptionsLiterals: Set[Literal] = alphabet.values.toSet -- assumptions

    val assumptionsArgs = bLitArgs.filter(arg => assumptions.contains(arg.lit)).toSet[Argument]

    completePiecesRecB(assumptionsArgs)
  }

// TODO:
//  def unexpandedPStatements(implicit dState: DisputeState): Set[Literal] = {
//    pLitArgs.map(_.lit).diff(pRuleArgs.map(_.rule.head)) // diff = filterNot + contains
//  }

  def unexpandedPStatements(implicit dState: DisputeState): Set[Argument] = {
    pLitArgs.filterNot(litArg => pRuleArgs.map(_.rule.head).contains(litArg.lit)).toSet[Argument]
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
      val newLitArgs = fullyExpandedStatementsLitArgs.filterNot(arg => (bRuleArgs -- playedBlockedRuleArgs).map(_.rule.head).contains(arg.lit)) -- playedBlockedLitArgs
      val newRuleArgs = bRuleArgs.filter(arg => arg.rule.body.intersect(playedBlockedLitArgs.map(_.lit)).nonEmpty) -- playedBlockedRuleArgs

      if (newRuleArgs.isEmpty && newLitArgs.isEmpty) args // do this as long as new arguments are created
      else playedBlockedPiecesRec(args ++ newRuleArgs ++ newLitArgs)
    }

    implicit val fullyExpandedStatementsLitArgs: Set[LiteralArgument] = bLitArgs.filter(arg => fullyExpandedStatements.contains(arg.lit))

    val culpritArgs = bLitArgs.filter(arg => culprits.contains(arg.lit)).toSet[Argument]

    playedBlockedPiecesRec(culpritArgs)
  }

  def culpritsCandidates(implicit dState: DisputeState): Set[Literal] = (assumptions intersect bLitArgs.map(_.lit)) -- (defences ++ culprits)


  def decorateAssumptions(implicit dState: DisputeState): Seq[(Literal, String)] = {
    // TODO: improve performance
    def isProponentAssumption(lit: Literal): String = if (dState.pLitArgs.map(_.lit).contains(lit)) "&" else ""
    // TODO:
    def isOpponentAssumption(lit: Literal): String = if ((dState.bLitArgs -- dState.pLitArgs).map(_.lit).contains(lit)) "@" else ""
    def isBlockedForP(lit: Literal): String = if (blockedAssumptionsP.contains(lit)) "~" else ""
    def isCulprit(lit: Literal): String = if (culprits.contains(lit)) "--" else ""

    val decFunctions = isProponentAssumption _ :: isOpponentAssumption _ :: isBlockedForP _ :: isCulprit _ :: Nil

    assumptions.toSeq.sortBy(_.id).map( ass =>
      (ass, decFunctions.foldLeft("")((currDec, func) => currDec + func(ass)) + ass)
    )
  }


  def decorateRules(implicit dState: DisputeState): Seq[(Rule, String)] = {
    // TODO: performance

    //val inconsistentRules = rules.filter(rule => contrariesOf(rule.body + rule.head).intersect(rule.body).nonEmpty)

    def isRuleUsedByP(rule: Rule): String = if (dState.pRuleArgs.map(_.rule).contains(rule)) "&" else ""
    def isRuleUsedByOpponent(rule: Rule): String = if ((dState.bRuleArgs -- dState.pRuleArgs).map(_.rule).contains(rule)) "@" else ""
    // blocked because of inconsistencies, constraints / contraries of defences in body TODO: should head be tested as well?

    //def isRuleBlocked1(rule: Rule): String = if (contrariesOf(rule.body + rule.head).intersect(rule.body  + rule.head).nonEmpty ||
    //    (rule.body + rule.head).intersect(constraints ++ contrariesOf(defences)).nonEmpty) "~" else ""
    //def isRuleBlocked2(rule: Rule): String = if (rule.body.intersect(culprits).nonEmpty) "--" else ""
    // TODO: Martin doesn't check block1 if block 2 is true

    def isRuleBlocked(rule: Rule): String =
      if (rule.body.intersect(culprits).nonEmpty) "--"
      else if (contrariesOf(rule.body + rule.head).intersect(rule.body  + rule.head).nonEmpty || (rule.body + rule.head).intersect(constraints ++ contrariesOf(defences)).nonEmpty) "~"
      else ""

    //val decFunctions = isRuleUsedByP _ :: isRuleUsedByOpponent _ :: isRuleBlocked1 _ :: isRuleBlocked2 _ :: Nil
    val decFunctions = isRuleUsedByP _ :: isRuleUsedByOpponent _ :: isRuleBlocked _ :: Nil

    rules.toSeq.sortBy(_.head.id).map( rule =>
      (rule, decFunctions.foldLeft("")((currDec, func) => currDec + func(rule)) + rule)
    )

  }


  def decorateArguments(implicit dState: DisputeState): Seq[(Argument, String)] = {

    // TODO: for performance reasons, evaluate it once, do not calculate it every time
    val culprits_ = culprits


    def isProponentPiece(arg: Argument): String = if (dState.p.contains(arg)) "$" else ""
    def isCompletePiece(arg: Argument): String = if (completePiecesP.contains(arg)) "**" else if (completePiecesB.contains(arg)) "*" else ""
    def isUnexpandedStatementP(arg: Argument): String = if (unexpandedPStatements.contains(arg)) "\"" else ""
    def isAssumptionOrFullyExpandedStatement(arg: Argument): String = {
      arg match {
        case litArg: LiteralArgument if ((assumptions ++ fullyExpandedStatements).contains(litArg.lit)) => "^"
        case _ => ""
      }
    }

    def isOpponentAssOrDefContr(arg: Argument): String = arg match {
      case litArg: LiteralArgument if (contrariesOf(defences).contains(litArg.lit)
        || (dState.bLitArgs.diff(dState.pLitArgs).contains(litArg) && assumptions.contains(litArg.lit))) => "!"
      case _ => ""
    }

    val decFunctions = isProponentPiece _ :: isCompletePiece _ :: isUnexpandedStatementP _ :: isAssumptionOrFullyExpandedStatement _ ::
      isOpponentAssOrDefContr _ :: Nil

    dState.b.toSeq.sortBy {
      case litArg: LiteralArgument => litArg.lit.id
      case ruleArg: RuleArgument => ruleArg.rule.head.id
    }.map( arg =>
      (arg, decFunctions.foldLeft("")((currDec, func) => currDec + func(arg)) + arg)
    )
  }

  def disputeStateToString(implicit dState: DisputeState): String = {
    // TODO: is it OK to assume that always a complete argument is a LiteralArgument?
    val completePiecesLits = completePiecesP.collect {
      case litArg: LiteralArgument => litArg.lit
      case ruleArg: RuleArgument => ruleArg.rule.head   // TODO?
    }

    def sortSetOfLiterals(literals: Set[Literal]): Seq[Literal] = literals.toSeq.sortBy(_.id)

    val goalsAndCulprContrWOCompleteArgs = goals ++ contrariesOf(culprits) -- completePiecesLits

    s"({${decorateArguments.map(_._2).mkString(" ; ")}}, " +
      s"{${sortSetOfLiterals(goalsAndCulprContrWOCompleteArgs).mkString(" ; ")}}, " +
      s"{${sortSetOfLiterals(defences).mkString(" ; ")}}, " +
      s"{${sortSetOfLiterals(culprits).mkString(" ; ")}})"
  }

  def checkIfOver(implicit dState: DisputeState, possibleMoves: Map[MoveType, Seq[PotentialMove]]): Option[Boolean] = {
    // TODO: consider making a function for that? or implicit conversion?
    val completeLiteralsB = completePiecesB.collect { case litArg: LiteralArgument => litArg }.map(_.lit)
    val playedBlockedLiterals = playedBlockedPieces.collect { case litArg: LiteralArgument => litArg }.map(_.lit)
    val completeLiteralsP = completePiecesP.collect { case litArg: LiteralArgument => litArg }.map(_.lit)
    if (contrariesOf(culprits).intersect(completeLiteralsB).subsetOf(playedBlockedLiterals)
      && (goals ++ contrariesOf(culprits)).subsetOf(completeLiteralsP)
      && possibleMoves.keys.forall(_.isProponentMove))
        //Some("Dispute over. Proponent wins")
        Some(true)
    else if ((contrariesOf(defences).intersect(completeLiteralsB).diff(playedBlockedLiterals).nonEmpty
      || (goals ++ contrariesOf(culprits)).diff(completeLiteralsP).nonEmpty)
        && (possibleMoves.keys.filter(_.isProponentMove).groupBy(_.isBackwardMove).size != 2)) // contains at least one forward and one backward prop move
        // Some("Dispute over. Opponent wins")
        Some(false)
    else
      None
  }

}