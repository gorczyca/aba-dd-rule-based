package aba.reasoner

import aba.framework.{Contrary, Framework, Rule}
import aba.move.Move.MoveType

case class MovePieces(newPRules: Set[Rule],
                      newPStatements: Set[String],
                      newORules: Set[Rule],
                      newOStatements: Set[String],
                      newDefences: Set[String],
                      newCulprits: Set[String])


trait PotentialMove2 {
  def moveType: MoveType

  def attacking: Option[Set[String]] // TODO: will that be necessary?
  def movePieces(implicit framework: Framework): MovePieces
  def perform(implicit dState: DisputeState, framework: Framework): DisputeState = {

      // TODO: somehow organize this
      val mPieces = movePieces

      val pRules = dState.pRules ++ mPieces.newPRules
      val pStatements = dState.pStatements ++ mPieces.newPStatements

      val oRules = dState.oRules ++ mPieces.newORules
      val oStatements = dState.oStatements ++ mPieces.newOStatements

      val bRules = oRules ++ pRules
      val bStatements = oStatements ++ pStatements // TODO: consider optimizing, depending on the move

      //val (ruleBodyAssumptions, ruleBodyNonAssumptions) = rule.body.partition(st => framework.assumptions.contains(st))
      //val newCulprits = framework.contraries.filter(ctr => rule.statements.contains(ctr.contrary)).map(_.assumption)
      val culprits = dState.culprits ++ mPieces.newCulprits
      val defences = dState.defences ++ mPieces.newDefences
      val defencesContraries = framework.contrariesOf(defences)

      // move it all to functions
      val pPlayedUnexpandedStatements = // checked
        DisputeState.calculatePPlayedUnexpandedStatements(pStatements = pStatements, pRules = pRules)

      val pRemainingNonBlockedRules = // checked
        DisputeState.calculatePRemainingNonBlockedRules(pRemainingNonBlockedRules = dState.pRemainingNonBlockedRules, pRules = pRules, culprits = culprits, defencesContraries = defencesContraries, framework.constraints)

      val bRemainingNonBlockedRules = // checked
        DisputeState.calculateBRemainingNonBlockedRules(bRemainingNonBlockedRules = dState.bRemainingNonBlockedRules, bRules = bRules, culprits = culprits)

      val bFullyExpandedStatements = // checked
        DisputeState.calculateBFullyExpandedStatements(bRemainingNonBlockedRules = bRemainingNonBlockedRules, bStatements = bStatements)

      val (bPlayedBlockedStatements, bPlayedBlockedRules) = // OK
        DisputeState.calculateBPlayedBlockedPiecesRec(
          blockedStatements = dState.bPlayedBlockedStatements ++ culprits, // OK
          blockedRules = dState.bPlayedBlockedRules, // OK
          bRemainingStatementsToCheck = (bFullyExpandedStatements -- dState.bPlayedBlockedStatements) -- framework.assumptions, // OK
          bRemainingRulesToCheck = bRules -- dState.bPlayedBlockedRules) // OK

     val (pPlayedCompleteStatements, pPlayedCompleteRules) = // OK
        DisputeState.calculatePPlayedCompletePiecesRec(
          pCompleteStatements = dState.pPlayedCompleteStatements ++ defences, // OK
          pCompleteRules = dState.pPlayedCompleteRules, // OK
          pRemainingRulesToCheck = pRules -- dState.pPlayedCompleteRules) // OK

      val pStatementsFollowingFromPCompleteStatements = pRemainingNonBlockedRules.filter{
        case Rule(_, head, body) => body.subsetOf(pPlayedCompleteStatements)
      }.map(_.head)

      val playedBNonBlockedStatements = bStatements -- bPlayedBlockedStatements
      val playedBNonBlockedRules = bRules -- bPlayedBlockedRules

      // unblocked complete pieces - TODO: again only if new culprits appear
      val nonCulpritsPlayedAssumptions = DisputeState.calculateNonCulpritsPlayedAssumptions(bStatements, framework.assumptions, culprits)

      val (bUnblockedCompletePlayedStatements, bUnblockedCompletePlayedRules) = // OK
        DisputeState.calculateBUnblockedCompletePiecesRec(
          bUnblockedCompleteStatements = nonCulpritsPlayedAssumptions ++ pPlayedCompleteStatements, // OK
          bUnblockedCompleteRules = pPlayedCompleteRules, // OK
          bRemainingStatementsToCheck = (playedBNonBlockedStatements -- pPlayedCompleteStatements) -- nonCulpritsPlayedAssumptions, // OK
          bRemainingRulesToCheck = playedBNonBlockedRules -- pPlayedCompleteRules) // OK

      // supporting defence contraries
      val initialStatementsSupportingDefenceContraries = playedBNonBlockedStatements intersect defencesContraries
      val (bUnblockedStatementsSupportingDefenceContraries,
        bUnblockedRulesSupportingDefenceContraries) =
          DisputeState.calculateBUnblockedPiecesSupportingStatementsInSet( // OK
            bSupportingStatements = initialStatementsSupportingDefenceContraries, // OK
            bSupportingRules = Set.empty, // OK
            bRemainingStatements = playedBNonBlockedStatements -- initialStatementsSupportingDefenceContraries, // OK
            bRemainingRules = playedBNonBlockedRules) // OK

      // supporting defended assumptions contraries
      val assumptionsAttackingDefences = framework.contraries.filter { case Contrary(assumption, _) => defences.contains(assumption) }.map(_.contrary) intersect framework.assumptions
      val attackedByBUnblockedCompleteStatements = framework.contraries.filter(ctr => bUnblockedCompletePlayedStatements.contains(ctr.contrary)).map(_.assumption)
      val currentlyDefendedAssumptions = framework.assumptions -- (culprits ++ attackedByBUnblockedCompleteStatements ++
          framework.selfContradictingAssumptions ++ assumptionsAttackingDefences) // TODO: here check if not attacking defences / is self contradicting?
      val currentlyDefendedAssumptionsContraries = framework.contrariesOf(currentlyDefendedAssumptions)

      val initialStatementsSupportingDefendedAssumptionsContraries = playedBNonBlockedStatements intersect currentlyDefendedAssumptionsContraries
      val (bUnblockedStatementsSupportingDefendedAssumptionsContraries,
      bUnblockedRulesSupportingDefendedAssumptionsContraries) = DisputeState.calculateBUnblockedPiecesSupportingStatementsInSet(
        bSupportingStatements = initialStatementsSupportingDefendedAssumptionsContraries,
        bSupportingRules = Set.empty,
        bRemainingStatements = playedBNonBlockedStatements -- initialStatementsSupportingDefendedAssumptionsContraries,
        bRemainingRules = playedBNonBlockedRules)

      val culpritCandidates = bUnblockedStatementsSupportingDefenceContraries intersect framework.assumptions
      val culpritCandidatesContraries = framework.contrariesOf(culpritCandidates)

      DisputeState(
        pStatements = pStatements,
        pRules = pRules,
        oStatements = oStatements,
        oRules = oRules,
        defences = defences,
        culprits = culprits,
        pRemainingNonBlockedRules = pRemainingNonBlockedRules,
        bRemainingNonBlockedRules = bRemainingNonBlockedRules,
        pPlayedUnexpandedStatements = pPlayedUnexpandedStatements,
        bFullyExpandedStatements = bFullyExpandedStatements,
        bPlayedBlockedStatements = bPlayedBlockedStatements,
        bPlayedBlockedRules = bPlayedBlockedRules,
        pPlayedCompleteStatements = pPlayedCompleteStatements,
        pPlayedCompleteRules = pPlayedCompleteRules,
        bUnblockedCompletePlayedStatements = bUnblockedCompletePlayedStatements,
        bUnblockedCompletePlayedRules = bUnblockedCompletePlayedRules,
        bUnblockedStatementsSupportingDefenceContraries = bUnblockedStatementsSupportingDefenceContraries,
        bUnblockedRulesSupportingDefenceContraries = bUnblockedRulesSupportingDefenceContraries,
        bUnblockedStatementsSupportingDefendedAssumptionsContraries = bUnblockedStatementsSupportingDefendedAssumptionsContraries,
        bUnblockedRulesSupportingDefendedAssumptionsContraries = bUnblockedRulesSupportingDefendedAssumptionsContraries,
        culpritCandidates = culpritCandidates,
        currentlyDefendedAssumptions = currentlyDefendedAssumptions,
        defenceContraries = defencesContraries,
        culpritCandidatesContraries = culpritCandidatesContraries,
        currentlyDefendedAssumptionsContraries = currentlyDefendedAssumptionsContraries,
        pStatementsFollowingFromPCompleteStatements = pStatementsFollowingFromPCompleteStatements
      )
    }

  def newPieces: (Set[Rule], Set[String])
}


trait PotentialRuleMove extends PotentialMove2 {
  val rule: Rule

  override def newPieces: (Set[Rule], Set[String]) = (Set(rule), rule.statements)
  override def toString: String = s"$moveType: $rule"
}

trait PotentialAssumptionMove extends PotentialMove2 {
  val assumption: String
  override def newPieces: (Set[Rule], Set[String]) = (Set.empty, Set(assumption)) // TODO: Change to a class
  override def toString: String = s"$moveType: $assumption"
}


trait ProponentMove extends PotentialMove2 {
  override def movePieces(implicit framework: Framework): MovePieces = {
    MovePieces(
      newPRules = newPieces._1,
      newPStatements =  newPieces._2,
      newORules = Set.empty,
      newOStatements = Set.empty,
      newDefences = newPieces._2 intersect framework.assumptions,
      newCulprits = framework.contraries.filter(ctr => newPieces._2.contains(ctr.contrary)).map(_.assumption)
    )
  }
}

trait OpponentMove extends PotentialMove2 {
  override def movePieces(implicit framework: Framework): MovePieces = {
    MovePieces(
      newPRules = Set.empty,
      newPStatements = Set.empty,
      newORules = newPieces._1,
      newOStatements = newPieces._2,
      newDefences = Set.empty,
      newCulprits = Set.empty
    )
  }
}

trait NonAttackingMove extends PotentialMove2 {
  override val attacking: Option[Set[String]] = None
}
