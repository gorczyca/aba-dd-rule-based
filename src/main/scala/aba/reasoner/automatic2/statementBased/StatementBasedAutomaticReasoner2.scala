package aba.reasoner.automatic2.statementBased

import aba.framework.Framework
import aba.move.DisputeAdvancement.DisputeAdvancementType
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.TerminationCriteria.TerminationCriteriaType
import aba.move.{OB1Move, OB2Move, OF1Move, OF2Move, PB1Move, PB2Move, PF1Move, PF2Move}
import aba.reasoner.automatic2.RuleChoice2.{LookAhead1Step, NewlyIntroducedAssumptionsMax, NewlyIntroducedStatementsMin, RuleChoiceType2}
import aba.reasoner.{PotentialAssumptionMove, PotentialMove2, PotentialRuleMove}
import aba.reasoner.automatic2.{AutomaticReasoner2, DisputeStateAuto2, RuleChoice2}
import aba.reasoner.automatic2.statementBased.StatementBasedAutomaticReasoner2.MapStatementsGrouped
import aba.reasoner.automatic2.statementBased.StatementChoice2.{LBCEager, LBCPatient, Patient, StatementChoiceType2}
import aba.reasoner.automatic2.statementBased.TurnChoice2.{Opponent, Proponent, TurnChoiceType2}



object StatementBasedAutomaticReasoner2 {
  type MapStatementsGrouped = Map[String, Iterable[PotentialMove2 with Product]]
  type MovesGrouped = Iterable[PotentialMove2]

  def default(terminationCriteriaType: TerminationCriteriaType,
              disputeAdvancementType: DisputeAdvancementType): StatementBasedAutomaticReasoner2 = {
    new StatementBasedAutomaticReasoner2(
        dfs = true, // dfs
        tCriteriaType = terminationCriteriaType, // tc
        dAdvancementType = disputeAdvancementType, // da
        startWithAdmissible = true, //swa
        turnChoice = Proponent, // turn
        pStatementChoice = LBCPatient, // psc
        oStatementChoice = LBCPatient, // osc
        pRuleChoiceType = NewlyIntroducedStatementsMin, // prc
        oRuleChoiceType = NewlyIntroducedStatementsMin // orc
      )

    // TODO: temporarily
  }



}


case class StatementBasedAutomaticReasoner2(dfs: Boolean,
                                            tCriteriaType: TerminationCriteriaType,
                                            dAdvancementType: DisputeAdvancementType,
                                            startWithAdmissible: Boolean,
                                            turnChoice: TurnChoiceType2,
                                            pStatementChoice: StatementChoiceType2,
                                            oStatementChoice: StatementChoiceType2,
                                            pRuleChoiceType: RuleChoiceType2,
                                            oRuleChoiceType: RuleChoiceType2) extends AutomaticReasoner2(dfs, dAdvancementType, tCriteriaType, startWithAdmissible) {


  override protected def generateNewDisputeStates(implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                                                  framework: Framework,
                                                  dStateAuto: DisputeStateAuto2): List[DisputeStateAuto2] = {

    val (proponentMovesGrouped, opponentMovesGrouped) = createStatementsMap
    val (movesGroupedSet, proponentChosen) = TurnChoice2(turnChoice)(proponentMovesGrouped, opponentMovesGrouped)

    val statementChoice = if (proponentChosen) pStatementChoice else oStatementChoice
    val statementsSequence = createStatementsSequence(proponentChosen, dStateAuto.performedMoves)
    val remainingRules = if (proponentChosen) dStateAuto.dState.pRemainingNonBlockedRules else dStateAuto.dState.bRemainingNonBlockedRules
    val statement = StatementChoice2(statementChoice)(movesGroupedSet.keySet, remainingRules, framework, statementsSequence)

    val movesToChooseFrom = movesGroupedSet(statement)

    getNewDisputeStates(movesToChooseFrom, statement, proponentChosen)

  }

  private def getNewDisputeStates(movesToChoose: Iterable[PotentialMove2], statement: String, proponentChosen: Boolean)
                                 (implicit currentDStateAuto: DisputeStateAuto2, framework: Framework): List[DisputeStateAuto2] = {


    val currentDState = currentDStateAuto.dState

    val isStatementAssumption = framework.assumptions.contains(statement)
    val isStatementInP = currentDState.pStatements.contains(statement)
    val isStatementInO = currentDState.oStatements.contains(statement)

    // helpers
    val `propChosen` = true
    val `oppChosen` = false
    val `isAssumption` = true
    val `isNonAssumption` = false
    val `isInP` = true
    val `notInP` = false
    val `isInO` = true
    val `notInO` = false

    (proponentChosen, isStatementAssumption, isStatementInP, isStatementInO) match {
        // OB2 / OF2
        // attack of a prop. assumption using OB2 or OF2, pick any
      case (`propChosen`, `isAssumption`, `isInP`, _) =>


        // There can either be rule attacks OR a single assumption attack
        // TODO: remove this if
//        if (Set(OB2, OF2).subsetOf(movesToChoose.map(_.moveType).toSet)) {
//          println("ERROR both OB2 and OF2 appeared!")
//        }
//
//        if (movesToChoose.count(_.moveType == OF2) > 1) {
//          println("ERROR more than one OF2 move!")
//        }

        // for flat frameworks it is impossible here to be both OB2 AND OF2
        val ob2Moves = movesToChoose.filter(_.moveType == OB2)
        val move =
          if (ob2Moves.nonEmpty) RuleChoice2(oRuleChoiceType)(ob2Moves, currentDState.bStatements, currentDState, framework, tCriteriaType).head
          // there must be exactly one OF2 move
          else movesToChoose.head

        List(DisputeStateAuto2(move))

        // PF2 / OB2 / OF2
        // adding an assumption to P: taking or ignoring OR attacking "currently def ass" by opp
      case (`propChosen`, `isAssumption`, `notInP`, _) =>
        // given a statement, either attack it OR take it to P, no other option

        // take 1 prop and 1 opp
        val propMoveOpt = movesToChoose.find(_.moveType.isProponentMove) // taking this to P (should be maximally one)
        val opponentMoves = movesToChoose.filter(_.moveType.isOpponentsMove)
        val ob2Moves = opponentMoves.filter(_.moveType == OB2)
        val oppMoveOpt =
          if (ob2Moves.nonEmpty) RuleChoice2(oRuleChoiceType)(ob2Moves, currentDState.bStatements, currentDState, framework, tCriteriaType).headOption
          // there must be exactly 1 OF2 move
          else opponentMoves.headOption

        (propMoveOpt, oppMoveOpt) match {
          case (Some(moveP), Some(moveO)) => List(
            DisputeStateAuto2(moveP),
            DisputeStateAuto2(moveO).copy(ignoredProponentAssumptions = currentDStateAuto.ignoredProponentAssumptions + statement)
          )
          //case (None, Some(moveO)) => List(DisputeStateAuto2(moveO))
          case (Some(moveP), None) =>
            List(currentDStateAuto.copy(ignoredProponentAssumptions = currentDStateAuto.ignoredProponentAssumptions + statement),
              DisputeStateAuto2(moveP))
            // TODO: there should be no other cases???
          case (None, Some(moveO)) =>
            // TODO:
            //  only if ignored statement is ignored by P. but then i think we also should get rid of attacking, but im not sure now
            List(DisputeStateAuto2(moveO))
        }

        // PB1 / PF1
        // expansion, either PB1 or PF1, always prefer PF1
      case (`propChosen`, `isNonAssumption`, _, _) =>

        // TODO: maybe here also make choice ???

        movesToChoose.find(_.moveType == PF1) match {
          case Some(move) => List(DisputeStateAuto2(move))
          case _ => // there is no PF1 move, only PB1
            RuleChoice2(pRuleChoiceType)(movesToChoose, currentDState.pStatements, currentDState, framework, tCriteriaType).map(DisputeStateAuto2(_))
        }

        // PB2 / PF2
        // attacking culprit candidates / ignoring them
      case (`oppChosen`, `isAssumption`, _, `isInO`) =>

        // there can either be exactly one PF2 move or a list of PB2 moves

        val pb2Moves = movesToChoose.filter(_.moveType == PB2)
        val moves =
          if (pb2Moves.nonEmpty) RuleChoice2(pRuleChoiceType)(pb2Moves, currentDState.pStatements, currentDState, framework, tCriteriaType).map(DisputeStateAuto2(_))
          // otherwise exactly one PF2 move
          else movesToChoose.map(DisputeStateAuto2(_))

        val ignoringMove = currentDStateAuto.copy(ignoredCulpritCandidates = currentDStateAuto.ignoredCulpritCandidates + statement)

        (Seq(ignoringMove) ++ moves).toList

        // PB2 attacking something not in O // TODO: in which cases is that possible?
        // PB2, PF2
        // OF2 taking an asm to O (which is not attacking anything) - just take the assumption
      case (`oppChosen`, `isAssumption`, _, _) =>

        // in DF, there might appear OF2 moves which not attack any other. Perform them
        movesToChoose.find(_.moveType == OF2) match {
          case Some(move) => List(DisputeStateAuto2(move))
          case _ =>

            val pb2Moves = movesToChoose.filter(_.moveType == PB2)
            val moves =
              if (pb2Moves.nonEmpty) RuleChoice2(pRuleChoiceType)(pb2Moves, currentDState.pStatements, currentDState, framework, tCriteriaType).map(DisputeStateAuto2(_))
              else movesToChoose.map(DisputeStateAuto2(_))

            val ignoringMove = currentDStateAuto.copy(ignoredCulpritCandidates = currentDStateAuto.ignoredCulpritCandidates + statement)

            (Seq(ignoringMove) ++ moves).toList
        }

       // either OB1 or OF1, always prefer Of1
      case (`oppChosen`, `isNonAssumption`, _, _) =>
        movesToChoose.find(_.moveType == OF1) match {
        case Some(move) => List(DisputeStateAuto2(move))
        case _ =>
          // OB1
          val move = RuleChoice2(oRuleChoiceType)(movesToChoose, currentDState.bStatements, currentDState, framework, tCriteriaType).head
          List(DisputeStateAuto2(move)) // there is no OF1 move, only PB1, take any
      }
    }
  }


  private def createStatementsMap(implicit possibleMoves: Map[MoveType, Seq[PotentialMove2]], dStateAuto: DisputeStateAuto2)// TODO: remove this with Prodcut from here
  : (MapStatementsGrouped, MapStatementsGrouped) = {

    val (proponentMoves, opponentMoves) = possibleMoves.values.flatten.partition {
      case PB1Move(_, _) | PF1Move(_, _) | PF2Move(_, None) | OB2Move(_, _) | OF2Move(_, Some(_)) => true
      case PB2Move(_, _) | PF2Move(_, Some(_)) | OB1Move(_, _) | OF1Move(_, _) | OF2Move(_, None) => false
      // for the 2nd case it would be enough _ =>, but explicitly said for clarity
    }

    val proponentMovesGroupedByStatements = proponentMoves.flatMap {
      case m@PB1Move(rule, _) => List((rule.head, m))
      case m@PF1Move(rule, _) => List((rule.head, m))
      case m@PF2Move(asm, _) => List((asm, m)).filter { case (asm, _) => !dStateAuto.ignoredProponentAssumptions.contains(asm) }  // TODO: check this as well, but should be fine
      //case m@OB2Move(_, Some(attackedAssumptions)) => (attackedAssumptions -- (dStateAuto.ignoredCurrentlyDefendedAssumptions -- dStateAuto.dState.defences)).map((_, m))
      case m@OB2Move(_, Some(attackedAssumptions)) => attackedAssumptions.map((_, m))
      //case m@OF2Move(_, Some(attackedAssumptions)) => (attackedAssumptions -- (dStateAuto.ignoredCurrentlyDefendedAssumptions -- dStateAuto.dState.defences)).map((_, m))
      case m@OF2Move(_, Some(attackedAssumptions)) => attackedAssumptions.map((_, m))
    }.groupBy(_._1).map { case (k,v) => (k,v.map(_._2)) } // proponents moves: String -> potential moves

    val opponentsMovesGroupedByStatements = opponentMoves.flatMap {
      case m@OB1Move(rule, _) => List((rule.head, m))
      case m@OF1Move(rule, _) => List((rule.head, m))
      case m@OF2Move(asm, _) => List((asm, m)) // TODO: shouldnt none be here?
      case m@PB2Move(_, Some(attackedAssumptions)) => (attackedAssumptions -- dStateAuto.ignoredCulpritCandidates).map((_, m))
      case m@PF2Move(_, Some(attackedAssumptions)) => (attackedAssumptions -- dStateAuto.ignoredCulpritCandidates).map((_, m))
    }.groupBy(_._1).map { case (k,v) => (k,v.map(_._2)) } // opponents moves: String -> potential moves

     (proponentMovesGroupedByStatements, opponentsMovesGroupedByStatements)
  }

  def createStatementsSequence(proponent: Boolean, performedMoves: List[PotentialMove2]): List[String] = {
    val moves = performedMoves.filter(move => if (proponent) move.moveType.isProponentMove else move.moveType.isOpponentsMove)
    moves.flatMap {
      case m: PotentialRuleMove => m.rule.statements
      case m: PotentialAssumptionMove => List(m.assumption)
    }.distinct
  }




}


