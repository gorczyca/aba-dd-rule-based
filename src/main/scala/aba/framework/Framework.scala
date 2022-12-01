package aba.framework

import aba.fileParser.FileParser
import aba.move.Move.MoveType
import aba.reasoner.{DisputeState, PotentialMove2}
//import aba.reasoner.{Argument, DisputeState, LiteralArgument, PotentialMove, PotentialMove2, RuleArgument}

import scala.annotation.tailrec

// companion object
object Framework {
  def apply(inputType: String, filePath: String, goal: Option[String] = None): Framework = FileParser(inputType, filePath, goal)
}


// TODO: keep here the file path

case class Framework (val rules: Set[Rule],
                 val assumptions: Set[String],
                 val contraries: Set[Contrary],
                 var goals: Set[String], // TODO: temporary
                 var constraints: Set[String], // TODO: var temporary
                 //val alphabet: Set[String]
                ) {

  val alphabet: Set[String] = assumptions ++ contraries.flatMap(ctr => Set(ctr.contrary, ctr.assumption)) ++ goals ++ rules.flatMap(_.statements)

  //
  def isEvenPossible: (Option[Set[String]], Option[Set[String]]) = {
    // check if goals do not contain self-contradictory assumptions or assumptions which are constraints
    val selfContradictingGoals = goals intersect selfContradictingAssumptions
    val constrainedGoals = goals intersect constraints

    val selfCG = if (selfContradictingGoals.nonEmpty) Some(selfContradictingGoals) else None
    val constG = if (constrainedGoals.nonEmpty) Some(constrainedGoals) else None

    (selfCG, constG)
  }

  // TODO: this should be calculated only once
  val selfContradictingAssumptions: Set[String] = contraries.filter(ctr => ctr.contrary == ctr.assumption).map(_.assumption)

  // helpers
  def contrariesOf(statement: String): Set[String] = contraries.filter(_.assumption == statement).map(_.contrary)

  def contrariesOf(statements: Set[String]): Set[String] = statements.flatMap(contrariesOf)

//  def decorateAssumptions(implicit dState: DisputeState): Seq[(Literal, String)] = {
//    // TODO: improve performance
//    def isProponentAssumption(lit: Literal): String = if (defences.contains(lit)) "&" else ""
//    // TODO:
//    def isOpponentAssumption(lit: Literal): String = if ((dState.bLitArgs -- dState.pLitArgs).map(_.lit).contains(lit)) "@" else ""
//    def isBlockedForP(lit: Literal): String = if (blockedAssumptionsP.contains(lit)) "~" else ""
//    def isCulprit(lit: Literal): String = if (culprits.contains(lit)) "--" else ""
//
//    val decFunctions = isProponentAssumption _ :: isOpponentAssumption _ :: isBlockedForP _ :: isCulprit _ :: Nil
//
//    assumptions.toSeq.sortBy(_.id).map( ass =>
//      (ass, decFunctions.foldLeft("")((currDec, func) => currDec + func(ass)) + ass)
//    )
//  }


//  def decorateRules(implicit dState: DisputeState): Seq[(Rule, String)] = {
//    // TODO: performance
//
//    //val inconsistentRules = rules.filter(rule => contrariesOf(rule.body + rule.head).intersect(rule.body).nonEmpty)
//
//    def isRuleUsedByP(rule: Rule): String = if (dState.pRuleArgs.map(_.rule).contains(rule)) "&" else ""
//    def isRuleUsedByOpponent(rule: Rule): String = if ((dState.bRuleArgs -- dState.pRuleArgs).map(_.rule).contains(rule)) "@" else ""
//    // blocked because of inconsistencies, constraints / contraries of defences in body TODO: should head be tested as well?
//
//    def isRuleBlocked1(rule: Rule): String = if (contrariesOf(rule.body + rule.head).intersect(rule.body  + rule.head).nonEmpty ||
//        (rule.body + rule.head).intersect(constraints ++ contrariesOf(defences)).nonEmpty) "~" else ""
//    def isRuleBlocked2(rule: Rule): String = if (rule.body.intersect(culprits).nonEmpty) "--" else ""
//
//    val decFunctions = isRuleUsedByP _ :: isRuleUsedByOpponent _ :: isRuleBlocked1 _ :: isRuleBlocked2 _ :: Nil
//    //val decFunctions = isRuleUsedByP _ :: isRuleUsedByOpponent _ :: isRuleBlocked _ :: Nil
//
//    rules.toSeq.sortBy(_.head.id).map( rule =>
//      (rule, decFunctions.foldLeft("")((currDec, func) => currDec + func(rule)) + rule)
//    )
//
//  }


//  def decorateArguments(implicit dState: DisputeState): Seq[(Argument, String)] = {
//
//    def isProponentPiece(arg: Argument): String = if (dState.p.contains(arg)) "$" else ""
//    def isCompletePiece(arg: Argument): String = if (completePiecesP.contains(arg)) "**" else if (completePiecesB.contains(arg)) "*" else ""
//    def isUnexpandedStatementP(arg: Argument): String = arg match {
//      case litArg: LiteralArgument if unexpandedPStatements.contains(litArg.lit) => "\""
//      case _ => ""
//    }
//
//    def isPlayedBlockedPiece(arg: Argument): String = if (playedBlockedPieces.contains(arg)) "--" else ""
//
//    def isAssumptionOrFullyExpandedStatement(arg: Argument): String = {
//      arg match {
//        case litArg: LiteralArgument if ((assumptions ++ fullyExpandedStatements).contains(litArg.lit)) => "^"
//        case _ => ""
//      }
//    }
//
//    def isOpponentAssOrDefContr(arg: Argument): String = arg match {
//      case litArg: LiteralArgument if (contrariesOf(defences).contains(litArg.lit)
//        || (dState.bLitArgs.diff(dState.pLitArgs).contains(litArg) && assumptions.contains(litArg.lit))) => "!"
//      case _ => ""
//    }
//
//    val decFunctions = isProponentPiece _ :: isCompletePiece _ :: isUnexpandedStatementP _ :: isAssumptionOrFullyExpandedStatement _ ::
//      isOpponentAssOrDefContr _ :: isPlayedBlockedPiece _ :: Nil
//
//    dState.b.toSeq.sortBy {
//      case litArg: LiteralArgument => litArg.lit.id
//      case ruleArg: RuleArgument => ruleArg.rule.head.id
//    }.map( arg =>
//      (arg, decFunctions.foldLeft("")((currDec, func) => currDec + func(arg)) + arg)
//    )
//  }

//  def disputeStateToString(argumentsOptString: Option[String] = None)(implicit dState: DisputeState): String = {
//    // TODO: is it OK to assume that always a complete argument is a LiteralArgument?
//    val completePiecesLits = completePiecesP.collect {
//      case litArg: LiteralArgument => litArg.lit
//      case ruleArg: RuleArgument => ruleArg.rule.head   // TODO?
//    }
//
//    def sortSetOfLiterals(literals: Set[Literal]): Seq[Literal] = literals.toSeq.sortBy(_.id)
//
//    val goalsAndCulprContrWOCompleteArgs = goals ++ contrariesOf(culprits) -- completePiecesLits
//
//    val argsString = argumentsOptString match {
//      case Some(string) => string
//      case _ => decorateArguments.map(_._2).mkString("; ")
//    }
//
//    s"B:\n\t{$argsString}" +
//    s"\nGoals & culprit contraries (w/o complete pieces):\n\t{${sortSetOfLiterals(goalsAndCulprContrWOCompleteArgs).mkString("; ")}}" +
//    s"\nDefences:\n\t{${sortSetOfLiterals(defences).mkString("; ")}}" +
//    s"\nCulprits:\n\t{${sortSetOfLiterals(culprits).mkString("; ")}}"
//  }

  // TODO: remove
  def checkIfOver(implicit dState: DisputeState, possibleMoves: Map[MoveType, Seq[PotentialMove2]]): Option[Boolean] = {
    // TODO: consider making a function for that? or implicit conversion?
    // previously we had
    //val completeLiteralsB = completePiecesB.collect { case litArg: LiteralArgument => litArg }.map(_.lit)
    //val playedBlockedLiterals = playedBlockedPieces.collect { case litArg: LiteralArgument => litArg }.map(_.lit)
    // TODO: move it to dState

    val culpritsContraries = contrariesOf(dState.culprits)

    val cond1 = (dState.defenceContraries  intersect dState.bUnblockedCompletePlayedStatements).isEmpty
    val cond2 = (goals ++ culpritsContraries).subsetOf(dState.pPlayedCompleteStatements)
    val cond3 = (possibleMoves.keys.filter(_.isOpponentsMove).groupBy(_.isBackwardMove).size != 2)


    if ((dState.defenceContraries intersect dState.bUnblockedCompletePlayedStatements).isEmpty
      && (goals ++ culpritsContraries).subsetOf(dState.pPlayedCompleteStatements)
      && (possibleMoves.keys.filter(_.isOpponentsMove).groupBy(_.isBackwardMove).size != 2)) // contains at least one forward and one backward prop move
        //Some("Dispute over. Proponent wins")
        Some(true)
    else if ((dState.defenceContraries intersect dState.bUnblockedCompletePlayedStatements).nonEmpty
      || (goals ++ culpritsContraries).diff(dState.pPlayedCompleteStatements).nonEmpty
        && (possibleMoves.keys.filter(_.isProponentMove).groupBy(_.isBackwardMove).size != 2)) // contains at least one forward and one backward prop move
        // Some("Dispute over. Opponent wins")
        Some(false)
    else
      None
  }

}