package aba.move

import aba.framework.Framework
import aba.move.DisputeAdvancement.{DC, DF, DisputeAdvancementType}
import aba.move.Move.{MoveType, OF2}
import aba.reasoner.{DisputeState, OpponentMove, PotentialAssumptionMove, PotentialMove2}

object OF2Move extends Move with OpponentsAttackingMove {

  private def possibleAssumptions(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Set[String] = {

    val possibleStatements = advancementType match {
      case DC => dState.defenceContraries ++ dState.currentlyDefendedAssumptionsContraries
      case DF => framework.assumptions // TODO: this shouldn't be calculated if AT other than DF
      case _ => dState.defenceContraries
    }
                                                                          // TODO: should probably also be done
    val nonUsedStatements = (possibleStatements diff dState.bStatements) //diff dState.culprits
    nonUsedStatements intersect framework.assumptions
  }

  override def isPossible(advancementType: DisputeAdvancementType)(implicit framework: Framework, dState: DisputeState): Seq[PotentialMove2] = {

    val assumptions = possibleAssumptions(advancementType) -- dState.culprits

    assumptions.map(ass => {
      OF2Move(ass, attackedAssumptionsBySet(Set(ass), advancementType))
    }).toSeq
  }
}

case class OF2Move(override val assumption: String,
                   override val attacking: Option[Set[String]],
                  ) extends PotentialAssumptionMove with OpponentMove {

  override val moveType: MoveType = OF2

}
