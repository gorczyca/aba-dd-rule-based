package aba.reasoner.automatic2.statementBased

import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2.MapStatementsGrouped

object TurnChoice2 extends Enumeration {

  type TurnChoiceType2 = Value

  val
    Proponent, // p
    Opponent, // o
    SmallestStatementSet, // s
    LargestStatementSet // l
  = Value


  def apply(turnChoiceType: TurnChoiceType2)(propMoves: MapStatementsGrouped, oppMoves: MapStatementsGrouped)
    : (MapStatementsGrouped, Boolean) = {

    turnChoiceType match {
      case Proponent => turnProponent(propMoves, oppMoves)
      case Opponent => turnOpponent(propMoves, oppMoves)
      case SmallestStatementSet => turnSmallestStatementSet(propMoves, oppMoves)
      case LargestStatementSet => turnLargestStatementSet(propMoves, oppMoves)
    }
  }

  private def turnProponent(propMoves: MapStatementsGrouped, oppMoves: MapStatementsGrouped): (MapStatementsGrouped, Boolean) =
    if (propMoves.nonEmpty) (propMoves, true) else (oppMoves, false)

  private def turnOpponent(propMoves: MapStatementsGrouped, oppMoves: MapStatementsGrouped): (MapStatementsGrouped, Boolean) =
    if (oppMoves.nonEmpty) (oppMoves, false) else (propMoves, true)

  private def turnSmallestStatementSet(propMoves: MapStatementsGrouped, oppMoves: MapStatementsGrouped): (MapStatementsGrouped, Boolean) = {
    if (propMoves.keySet.size <= oppMoves.keySet.size) { // if there is less for the proponent
      if (propMoves.keySet.nonEmpty) (propMoves, true) // if proponent even has any move
      else (oppMoves, false)
    } else if (oppMoves.keySet.nonEmpty) (oppMoves, false) // otherwise, less for opponent, if opponent has any, return it
    else (propMoves, true) // opponent has none, return proponent
  }

  private def turnLargestStatementSet(propMoves: MapStatementsGrouped, oppMoves: MapStatementsGrouped): (MapStatementsGrouped, Boolean) =
    if (propMoves.keySet.size >= oppMoves.keySet.size) { // if there is more for the the proponent
      if (propMoves.keySet.nonEmpty) (propMoves, true) // if proponent even has any move
      else (oppMoves, false)
    } else (oppMoves, false) // otherwise, more for opponent (opponent must not be empty)
}
