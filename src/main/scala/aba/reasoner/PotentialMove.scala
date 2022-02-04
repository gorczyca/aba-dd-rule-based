package aba.reasoner

import aba.framework.Literal
import aba.move.Move.MoveType


// TODO Here also sealed trait
// TODO and cased clases ruleArg and assumptionArg
case class PotentialMove(ruleArgument: Option[RuleArgument],
                         // literalArgument: Option[LiteralArgument],
                         assumptionArgument: Option[LiteralArgument],
                         literalArguments: Set[LiteralArgument],
                         moveType: MoveType,
                         additionalInfo: Option[String],
                         attacking: Option[Set[Literal]] = None) extends Ordered[PotentialMove] {
  override def compare(that: PotentialMove): Int = this.moveType compare that.moveType

  def perform(implicit dState: DisputeState): DisputeState = {

    val totalArguments: Set[Argument] = ruleArgument match {
      case Some(ruleArg) => literalArguments.toSet[Argument] + ruleArg
      case None => literalArguments.toSet[Argument]   // TODO: do it better than toSet[Argument]
    }

    val arg = (ruleArgument, assumptionArgument) match {
      case (Some(_), Some(_)) => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
      case (Some(ruleArg), None) => ruleArg
      case (None, Some(litArg)) => litArg
      case _ => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
    }

    DisputeState(moveType, totalArguments, arg)
  }


  override def toString: String = {

    // TODO: remove?
//    val additionalInfoStr = additionalInfo match {
//      case Some(addInfo) => s" ($addInfo)"
//      case _ => ""
//    }

//    val ruleArgStr = ruleArgument match {
//      case Some(ruleArg) => s"Rule: $ruleArg"
//      case _ => ""
//    }

    // TODO? previously was showing all literal arguments, probably just remove
    //val litArgStr = if (literalArguments.isEmpty) "" else s"Literals: ${literalArguments.mkString(",")}"

    //s"$moveType$additionalInfoStr: $ruleArgStr $litArgStr"

    val info1 = (ruleArgument, assumptionArgument) match {
      case (Some(_), Some(_)) => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
      case (Some(ruleArg), None) => s"Rule: $ruleArg"
      case (None, Some(litArg)) => s"Assumption: $litArg"
      case _ => throw new IllegalArgumentException("Potential argument must either be literal or rule based.")
    }
    val info2 = attacking match {
      case Some(asm: Set[Literal]) => s"""Attacking { ${asm.mkString(",")} }"""
      case _ => ""
    }

    info1 + "\t\t" +  info2

  }

  // to compare in set between sets of potential arguments
//  override def hashCode(): Int = (moveType.toString + this.toString).hashCode
//
//  override def equals(obj: Any): Boolean = {
//    obj match {
//      case PotentialMove(thatRuleArgOpt, thatAssArgOpt, _, thatMType, _) if thatMType == moveType => {
//        thatRuleArgOpt match {
//          case Some(thatRuleArg) => this.ruleArgument match {
//            case Some(ruleArg) => thatRuleArg.equals(ruleArg)
//            case _ => false
//          }
//          case None => thatAssArgOpt match {
//            case Some(thatAssArg) => this.assumptionArgument match {
//              case Some(assArg) => thatAssArg.equals(assArg)
//              case _ => false
//            }
//          }
//        }
//      }
//      case _ => false
//    }
//  }
}
