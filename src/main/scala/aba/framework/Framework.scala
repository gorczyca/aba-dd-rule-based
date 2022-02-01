package aba.framework

import aba.fileParser.FileParser
import aba.move.Move.MoveType
import aba.reasoner.{Argument, DisputeState, LiteralArgument, PotentialMove, RuleArgument}

import scala.annotation.tailrec

// companion object
object Framework {
  def apply(inputType: String, filePath: String): Framework = FileParser(inputType, filePath)
}


// TODO: keep here the file path

class Framework (val rules: Set[Rule],
                 val assumptions: Set[String],
                 val contraries: Set[Contrary],
                 val goals: Set[String],
                 val constraints: Set[String],
                 val alphabet: Set[String]
                ) {
  def initialDState: DisputeState = DisputeState(goals)

  // helpers
  def contrariesOf(statement: String): Set[String] = contraries.filter(_.assumption == statement).map(_.contrary)

  def contrariesOf(statements: Set[String]): Set[String] = statements.map(contrariesOf)


  // implicits
  @deprecated
  implicit class ArgumentsSetWrapper(val set: Set[Argument]) {
    @deprecated
    def litArgs: Set[LiteralArgument] = set.collect { case litArg: LiteralArgument => litArg }
    @deprecated
    def ruleArgs: Set[RuleArgument] = set.collect { case ruleArg: RuleArgument => ruleArg }
  }



  @deprecated
  def bRuleArgs(implicit dState: DisputeState): Set[RuleArgument] = dState.bRuleArgs

  @deprecated
  def bLitArgs(implicit dState: DisputeState): Set[LiteralArgument] = dState.bLitArgs

  @deprecated
  def pRuleArgs(implicit dState: DisputeState): Set[RuleArgument] = dState.pRuleArgs

  @deprecated
  def pLitArgs(implicit dState: DisputeState): Set[LiteralArgument] = dState.pLitArgs


  // Paper specific

  // TODO: make dStates implicit?
  @deprecated
  def defences(implicit dState: DisputeState): Set[Literal] = {
    pLitArgs.map(_.lit).intersect(assumptions)
  }

  @deprecated
  def culprits(implicit dState: DisputeState): Set[Literal] = {
    contraries.filter(ctr => pLitArgs.map(_.lit).contains(ctr.contrary)).map(_.assumption)
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
  // TODO: cant I just do completePiecesB intersect p ?
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

  def unexpandedPStatements(implicit dState: DisputeState): Set[Literal] = {
    pLitArgs.filterNot(litArg => pRuleArgs.map(_.rule.head).contains(litArg.lit)).map(_.lit)
  }

  def fullyExpandedStatements(implicit dState: DisputeState): Set[Literal] = {
    // TODO: previously had
    //bLitArgs.map(_.lit).diff(remainingRulesP.diff(blockedRulesB).map(_.head))
    bLitArgs.map(_.lit).diff(remainingNonBlockedBRules.map(_.head))
  }

  def playedBlockedPieces(implicit dState: DisputeState): Set[Argument] = {

    // TODO: what is the naming convention for wrapped recursive function?
    @tailrec
    def playedBlockedPiecesRec(args: Set[Argument])(implicit dState: DisputeState,
                                                    fullyExpandedStatementsLitArgs: Set[LiteralArgument]): Set[Argument] = {

      val playedBlockedRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }
      val playedBlockedLitArgs = args.collect { case litArg: LiteralArgument => litArg }

      // important:
      // the part: && !assumptions.contains(arg.lit) is crucial, since  then every played assumption becomes a blocked piece
      val newLitArgs = fullyExpandedStatementsLitArgs.filter(arg => !(dState.bRuleArgs -- playedBlockedRuleArgs).map(_.rule.head).contains(arg.lit) && !assumptions.contains(arg.lit)) -- playedBlockedLitArgs
      val newRuleArgs = bRuleArgs.filter(arg => arg.rule.body.intersect(playedBlockedLitArgs.map(_.lit)).nonEmpty) -- playedBlockedRuleArgs

      if (newRuleArgs.isEmpty && newLitArgs.isEmpty) args // do this as long as new arguments are created
      else playedBlockedPiecesRec(args ++ newRuleArgs ++ newLitArgs)
    }

    implicit val fullyExpandedStatementsLitArgs: Set[LiteralArgument] = bLitArgs.filter(arg => fullyExpandedStatements.contains(arg.lit))

    val culpritArgs = bLitArgs.filter(arg => culprits.contains(arg.lit)).toSet[Argument]

    playedBlockedPiecesRec(culpritArgs)
  }

  def unblockedCompletePlayedPiecesB(implicit dState: DisputeState): Set[Argument] = {


    @tailrec
    def unblockedCompletePlayedPiecesBRec(args: Set[Argument])(implicit dState: DisputeState,
                                                    playedNonBlockedRules: Set[RuleArgument],
                                                    playedNonBlockedNonAssumptionsLitArguments: Set[LiteralArgument]): Set[Argument] = {

      val playedUnblockedCompleteRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }
      val playedUnblockedCompleteLitArgs = args.collect { case litArg: LiteralArgument => litArg }

      val newRuleArgs = playedNonBlockedRules.filter(_.rule.body.subsetOf(playedUnblockedCompleteLitArgs.map(_.lit))) -- playedUnblockedCompleteRuleArgs
      val newLitArgs = playedNonBlockedNonAssumptionsLitArguments.filter(_.parents.intersect(args).nonEmpty) -- playedUnblockedCompleteLitArgs

      if (newLitArgs.isEmpty && newRuleArgs.isEmpty) args
      else unblockedCompletePlayedPiecesBRec(args ++ newRuleArgs ++ newLitArgs)

    }

    implicit val playedNonBlockedRules: Set[RuleArgument] = bRuleArgs -- playedBlockedPieces.collect { case ruleArg: RuleArgument => ruleArg }
    implicit val playedNonBlockedNonAssumptionsLitArguments: Set[LiteralArgument] = (bLitArgs -- playedBlockedPieces.collect { case litArg: LiteralArgument => litArg }).filterNot(litArg => assumptions.contains(litArg.lit))

    val nonCulpritAssumptions = assumptions -- culprits
    val playedNonCulpritAssumptions = dState.bLitArgs.filter(litArg => nonCulpritAssumptions.contains(litArg.lit)).toSet[Argument]

    unblockedCompletePlayedPiecesBRec(playedNonCulpritAssumptions)
  }

  def unblockedPiecesSupportingStatements(statements: Set[Literal])(implicit dState: DisputeState): Set[Argument] = {

    @tailrec
    def unblockedPiecesSupportingStatementsRec(args: Set[Argument])(implicit dState: DisputeState,
                                                                    nonBlockedPlayedLitArgs: Set[LiteralArgument],
                                                                    nonBlockedPlayedRuleArgs: Set[RuleArgument]): Set[Argument] = {

      val unblockedPiecesLitArgs = args.collect { case litArg: LiteralArgument => litArg }
      val unblockedPiecesRuleArgs = args.collect { case ruleArg: RuleArgument => ruleArg }

      val newLitArgs = nonBlockedPlayedLitArgs.filter(litArg => unblockedPiecesRuleArgs.exists(_.rule.body.contains(litArg.lit))) -- unblockedPiecesLitArgs
      val newRuleArgs = nonBlockedPlayedRuleArgs.filter(ruleArg => unblockedPiecesLitArgs.map(_.lit).contains(ruleArg.rule.head)) -- unblockedPiecesRuleArgs

      if (newLitArgs.isEmpty && newRuleArgs.isEmpty) args
      else unblockedPiecesSupportingStatementsRec(args ++ newRuleArgs ++ newLitArgs)
    }

    val playedBlockedPiecesLitArgs: Set[LiteralArgument] = playedBlockedPieces.collect { case litArg: LiteralArgument => litArg }
    val playedBlockedPiecesRuleArgs: Set[RuleArgument] = playedBlockedPieces.collect { case ruleArg: RuleArgument => ruleArg }

    implicit val nonBlockedPlayedLitArgs: Set[LiteralArgument] = bLitArgs -- playedBlockedPiecesLitArgs
    implicit val nonBlockedPlayedRuleArgs: Set[RuleArgument] = bRuleArgs -- playedBlockedPiecesRuleArgs

    val playedNonBlockedPieces = nonBlockedPlayedLitArgs.filter(litArg => statements.contains(litArg.lit)).toSet[Argument]

    unblockedPiecesSupportingStatementsRec(playedNonBlockedPieces)
  }

  // previously we had:
  //def culpritsCandidates(implicit dState: DisputeState): Set[Literal] = (assumptions intersect bLitArgs.map(_.lit)) -- (defences ++ culprits)
  def culpritsCandidates(implicit dState: DisputeState): Set[Literal] = assumptions intersect unblockedPiecesSupportingStatements(contrariesOf(defences)).collect { case litArg: LiteralArgument => litArg.lit }

  def criticalPieces(implicit dState: DisputeState): Set[Argument] = {

    @tailrec
    def criticalPiecesRec(args: Set[Argument])(
      implicit dState: DisputeState, nonBlockedPiecesLit: Set[LiteralArgument], nonBlockedPiecesRules: Set[RuleArgument]): Set[Argument] = {

      val criticalLitArgs = args.litArgs
      val criticalRuleArgs = args.ruleArgs

      val newLitArgs = nonBlockedPiecesLit.filter(litArg => criticalRuleArgs.exists(_.rule.body.contains(litArg.lit))) -- criticalLitArgs
      val newRuleArgs = nonBlockedPiecesRules.filter(ruleArg => criticalLitArgs.exists(_.lit == ruleArg.rule.head)) -- criticalRuleArgs

      if (newLitArgs.isEmpty && newRuleArgs.isEmpty) args
      else criticalPiecesRec(args ++ newLitArgs ++ newRuleArgs)
    }

    implicit val nonBlockedPiecesLit: Set[LiteralArgument] = bLitArgs -- playedBlockedPieces.litArgs
    implicit val nonBlockedPiecesRules: Set[RuleArgument] = bRuleArgs -- playedBlockedPieces.ruleArgs

    val nonBlockedDefencesContraries = nonBlockedPiecesLit.filter( litArg => contrariesOf(defences).contains(litArg.lit) ).toSet[Argument]

    criticalPiecesRec(nonBlockedDefencesContraries)
  }

  def j(implicit dState: DisputeState): Set[Literal] = {
    val unblockedCompleteBLit = unblockedCompletePlayedPiecesB.collect { case litArg: LiteralArgument => litArg.lit } // performance
    (assumptions -- culprits).filter(ass => contrariesOf(ass).intersect(unblockedCompleteBLit).isEmpty)
  }


  def decorateAssumptions(implicit dState: DisputeState): Seq[(Literal, String)] = {
    // TODO: improve performance
    def isProponentAssumption(lit: Literal): String = if (defences.contains(lit)) "&" else ""
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

    def isRuleBlocked1(rule: Rule): String = if (contrariesOf(rule.body + rule.head).intersect(rule.body  + rule.head).nonEmpty ||
        (rule.body + rule.head).intersect(constraints ++ contrariesOf(defences)).nonEmpty) "~" else ""
    def isRuleBlocked2(rule: Rule): String = if (rule.body.intersect(culprits).nonEmpty) "--" else ""

    // TODO: Martin doesn't check block1 if block 2 is true
//    def isRuleBlocked(rule: Rule): String =
//      if (rule.body.intersect(culprits).nonEmpty) "--"
//      else if (contrariesOf(rule.body + rule.head).intersect(rule.body  + rule.head).nonEmpty || (rule.body + rule.head).intersect(constraints ++ contrariesOf(defences)).nonEmpty) "~"
//      else ""

    val decFunctions = isRuleUsedByP _ :: isRuleUsedByOpponent _ :: isRuleBlocked1 _ :: isRuleBlocked2 _ :: Nil
    //val decFunctions = isRuleUsedByP _ :: isRuleUsedByOpponent _ :: isRuleBlocked _ :: Nil

    rules.toSeq.sortBy(_.head.id).map( rule =>
      (rule, decFunctions.foldLeft("")((currDec, func) => currDec + func(rule)) + rule)
    )

  }


  def decorateArguments(implicit dState: DisputeState): Seq[(Argument, String)] = {

    def isProponentPiece(arg: Argument): String = if (dState.p.contains(arg)) "$" else ""
    def isCompletePiece(arg: Argument): String = if (completePiecesP.contains(arg)) "**" else if (completePiecesB.contains(arg)) "*" else ""
    def isUnexpandedStatementP(arg: Argument): String = arg match {
      case litArg: LiteralArgument if unexpandedPStatements.contains(litArg.lit) => "\""
      case _ => ""
    }

    def isPlayedBlockedPiece(arg: Argument): String = if (playedBlockedPieces.contains(arg)) "--" else ""

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
      isOpponentAssOrDefContr _ :: isPlayedBlockedPiece _ :: Nil

    dState.b.toSeq.sortBy {
      case litArg: LiteralArgument => litArg.lit.id
      case ruleArg: RuleArgument => ruleArg.rule.head.id
    }.map( arg =>
      (arg, decFunctions.foldLeft("")((currDec, func) => currDec + func(arg)) + arg)
    )
  }

  def disputeStateToString(argumentsOptString: Option[String] = None)(implicit dState: DisputeState): String = {
    // TODO: is it OK to assume that always a complete argument is a LiteralArgument?
    val completePiecesLits = completePiecesP.collect {
      case litArg: LiteralArgument => litArg.lit
      case ruleArg: RuleArgument => ruleArg.rule.head   // TODO?
    }

    def sortSetOfLiterals(literals: Set[Literal]): Seq[Literal] = literals.toSeq.sortBy(_.id)

    val goalsAndCulprContrWOCompleteArgs = goals ++ contrariesOf(culprits) -- completePiecesLits

    val argsString = argumentsOptString match {
      case Some(string) => string
      case _ => decorateArguments.map(_._2).mkString("; ")
    }

    s"B:\n\t{$argsString}" +
    s"\nGoals & culprit contraries (w/o complete pieces):\n\t{${sortSetOfLiterals(goalsAndCulprContrWOCompleteArgs).mkString("; ")}}" +
    s"\nDefences:\n\t{${sortSetOfLiterals(defences).mkString("; ")}}" +
    s"\nCulprits:\n\t{${sortSetOfLiterals(culprits).mkString("; ")}}"
  }

  // TODO: remove
  def checkIfOver(implicit dState: DisputeState, possibleMoves: Map[MoveType, Seq[PotentialMove]]): Option[Boolean] = {
    // TODO: consider making a function for that? or implicit conversion?
    // previously we had
    //val completeLiteralsB = completePiecesB.collect { case litArg: LiteralArgument => litArg }.map(_.lit)
    //val playedBlockedLiterals = playedBlockedPieces.collect { case litArg: LiteralArgument => litArg }.map(_.lit)

    val unblockedCompletePlayedLits = unblockedCompletePlayedPiecesB.collect { case litArg: LiteralArgument => litArg }.map(_.lit)

    val completeLiteralsP = completePiecesP.collect { case litArg: LiteralArgument => litArg }.map(_.lit)

    val cond1 = contrariesOf(defences).intersect(unblockedCompletePlayedLits).isEmpty
    val cond2 = (goals ++ contrariesOf(culprits)).subsetOf(completeLiteralsP)
    val cond3 = (possibleMoves.keys.filter(_.isOpponentsMove).groupBy(_.isBackwardMove).size != 2)


    if (contrariesOf(defences).intersect(unblockedCompletePlayedLits).isEmpty
      && (goals ++ contrariesOf(culprits)).subsetOf(completeLiteralsP)
      && (possibleMoves.keys.filter(_.isOpponentsMove).groupBy(_.isBackwardMove).size != 2)) // contains at least one forward and one backward prop move
        //Some("Dispute over. Proponent wins")
        Some(true)
    else if ((contrariesOf(defences).intersect(unblockedCompletePlayedLits).nonEmpty
      || (goals ++ contrariesOf(culprits)).diff(completeLiteralsP).nonEmpty)
        && (possibleMoves.keys.filter(_.isProponentMove).groupBy(_.isBackwardMove).size != 2)) // contains at least one forward and one backward prop move
        // Some("Dispute over. Opponent wins")
        Some(false)
    else
      None
  }

}