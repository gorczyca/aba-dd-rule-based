package aba.reasoner

import aba.framework.{Framework, Rule}

object DisputeStateWrapper {

  def proponentRule(rule: Rule)(implicit disputeStateWrapper: DisputeStateWrapper, framework: Framework): DisputeStateWrapper = {
    ???
    val newStatements = rule.statements
    val previousDisputeState = disputeStateWrapper.disputeState
    val newDisputeState = DisputeState(
      previousDisputeState.pStatements ++ newStatements,
      previousDisputeState.pRules + rule,
      previousDisputeState.oStatements,
      previousDisputeState.oRules)

    // new culprits
    val newCulprits = framework.contraries.filter(_.contrary == rule.head).map(_.assumption)
    val newDefences = rule.body.intersect(framework.assumptions)

    // new non-blocked remaining rules
    disputeStateWrapper.bRemainingNonBlockedRules

    new DisputeStateWrapper(
      newDisputeState,
      disputeStateWrapper.pRemainingRules - rule,
      //disputeStateWrapper. - rule,



    )

  }

  def proponentAssumption(assumption: String)(implicit disputeStateWrapper: DisputeStateWrapper, framework: Framework): DisputeStateWrapper = {
    ???
  }

  def opponentRule(rule: Rule)(implicit disputeStateWrapper: DisputeStateWrapper, framework: Framework): DisputeStateWrapper = {
    ???
  }

  def opponentAssumption(assumption: String)(implicit disputeStateWrapper: DisputeStateWrapper, framework: Framework): DisputeStateWrapper = {
    ???
  }


}

// TODO: find better way to handle this many things
case class DisputeStateWrapper(disputeState: DisputeState,
                               pRemainingRules: Set[Rule],
                               //bRemainingRules: Set[Rule],
                               pBlockedRules: Set[Rule],
                               //bBlockedRules: Set[Rule],
                               pRemainingNonBlockedRules: Set[Rule],
                               bRemainingNonBlockedRules: Set[Rule],
                               pBlockedAssumptions: Set[String],
                               pCompletePieces: Set[String],
                               bCompletePieces: Set[String],
                               pUnexpandedStatements: Set[String],
                               fullyExpandedStatements: Set[String],
                               playedBlockedStatements: Set[String],
                               playedBlockedRules: Set[Rule],
                               bUnblockedCompletePlayedStatements: Set[String],
                               pUnblockedCompletePlayedRules: Set[Rule],
                              )
