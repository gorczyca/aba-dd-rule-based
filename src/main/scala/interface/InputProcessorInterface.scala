package interface

import aba.framework.Framework
import aba.move.DisputeAdvancement.{DAB, DF, DisputeAdvancementType, DC, DS, DABF}
import aba.move.TerminationCriteria.{TA, TC, TS}
import aba.move.Move.{MoveType, OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import aba.move.{DisputeAdvancement, Move, TerminationCriteria}
import aba.move.TerminationCriteria.TerminationCriteriaType
import aba.reasoner.automatic2.{AutomaticReasoner2, RuleChoice2}
import aba.reasoner.automatic2.movesPreferenceBased.MovesPreferenceBasedAutomaticReasoner2
import aba.reasoner.automatic2.movesPreferenceBased.RuleHeadChoice.{LeastRules, MostRules, RandomHead}
import aba.reasoner.automatic2.statementBased.StatementChoice2
import aba.reasoner.structured.StructuredReasoner
import aba.reasoner.{DisputeState, PotentialMove2}
import dot.DotConverter
import interface.dotConverters.ABRepresentationInterface.generateABRepresentation
import interface.dotConverters.RuleDotRepresentationInterface.generateRuleRepresentation

// reasoners interfaces
import interface.reasoners.ApproximateReasonerInterface.findSuccessfulApproximateDerivation
import interface.reasoners.InteractiveReasonerInterface.getNewStateInteractively
import interface.reasoners.AutomaticReasonerInterface.findSuccessfulDerivations2
import interface.reasoners.PreferredReasonerInterface.findSuccessfulDerivationsPreferred
import interface.reasoners.GroundedReasonerInterface.findSuccessfulDerivationsGrounded

// implicits
import interface.CustomImplicits._

import scala.annotation.tailrec
import scala.io.AnsiColor

object InputProcessorInterface {

  // TODO: green as a constant
  def getUserInput: String = {
    print(s"${AnsiColor.GREEN}> ")
    val input = Console.in.readLine
    print(AnsiColor.RESET)
    input
  }

  // TODO:
  private def updateReasoners(newReasoner: MovesPreferenceBasedAutomaticReasoner2)(implicit state: ProgramState): ProgramState = {
    state.copy(
      automaticReasoner = newReasoner,
      approximateReasoner = state.approximateReasoner.copy(automaticReasoner2 = newReasoner),
    )
  }

  def processUserInput(implicit state: ProgramState): ProgramState = {


    // TODO clean
    implicit val currentDState: DisputeState = state.currentDState
    implicit val currentDAdvancement: DisputeAdvancementType = state.dAdvancement
    implicit val currentTCriteria: TerminationCriteriaType = state.tCriteria
    implicit val possibleMoves: Map[MoveType, Seq[PotentialMove2]] = state.possibleMoves
    implicit val framework: Framework = state.framework

    val digitRegex = """\d+""".r
    val probabilityRegex = """^(0(\.[0-9]+)?|1(\.0+)?)$""".r


    val stateId = state.performedMoves.length

    val Seq(
      moveTypesStrings,
      advancementTypesStrings,
      terminationCriteriaTypesStrings
    )
    = Seq(Move, DisputeAdvancement, TerminationCriteria).map(_.values.map(_.toString.toLowerCase()))

    val possibleMoveTypesStrings = possibleMoves.keys.map(_.toString.toLowerCase()).toSet


    val newState = getUserInput match {

      case "struct" =>
        StructuredReasoner.run

      case s"approx ${p} ${o} ${m}" if probabilityRegex.matches(p) && probabilityRegex.matches(o) &&
        (m == "s" || m == "d") =>
        // TODO

        val static = m == "s"

        val propP = p.toDouble
        val oppP = o.toDouble
        val newApproxReasoner = state.approximateReasoner.copy(proponentP = propP, opponentP = oppP, static = static)

        val newState = state.copy(
          approximateReasoner = newApproxReasoner
        )

        val (perfMoves, newDState) = findSuccessfulApproximateDerivation(newState)

        state.copy(
          performedMoves = state.performedMoves ++ perfMoves,
          currentDState = newDState,
          redraw = false
        )

        //newState

      case "ip" =>
        getNewStateInteractively(proponentsMove = true)

      case "io" =>
        getNewStateInteractively(proponentsMove = false)

      case "auto" =>
        findSuccessfulDerivations2(onlyOne = true)

      //case "auto" => // TODO:
      //  findSuccessfulDerivations2(onlyOne = false)

      case "pref" =>
        // TODO: return the state, allow to find one only or more
        findSuccessfulDerivationsPreferred
        state

      case "grd" =>
        // TODO: return the state, allow to find one only or more
        findSuccessfulDerivationsGrounded()
        state
      case s"g $goal" =>
        state.copy(
          framework = framework.copy(goals = Set(goal))
        )
      case "strat" =>
       println( s"+-------------------+\n" +
                s"|  Chosen strategy: |\n" +
                s"+-------------------+\n"
                )
        println(state.automaticReasoner.toString)
        state.copy(redraw = false)

        // TODO: somehow propagate this to ALL reasoners
      case "dfs 1" =>
        println("Search type: DFS")
        val newReasoner = state.automaticReasoner.copy(dfs = true)
        updateReasoners(newReasoner)

      case "dfs 0" =>
        println("Search type: BFS")
        val newReasoner = state.automaticReasoner.copy(dfs = false)
        updateReasoners(newReasoner)

      case "swa 1" =>
        println("Start with adissible: YES")
        val newReasoner = state.automaticReasoner.copy(startWithAdmissible = true)
        updateReasoners(newReasoner)

      case "swa 0" =>
        println("Start with adissible: NO")
        val newReasoner = state.automaticReasoner.copy(startWithAdmissible = false)
        updateReasoners(newReasoner)

      case s"phc $x" =>
        val phc  = x.toLowerCase match {
          case "m" => MostRules
          case "l" => LeastRules
          case "r" => RandomHead
          case _ =>
            println("Invalid.")
            MostRules
        }

        println(s"Prop. rule head choice: $phc")
        val newReasoner = state.automaticReasoner.copy(pRuleHeadChoice = phc)
        updateReasoners(newReasoner)

      case s"ohc $x" =>
        val ohc  = x.toLowerCase match {
          case "m" => MostRules
          case "l" => LeastRules
          case "r" => RandomHead
          case _ =>
            println("Invalid.")
            MostRules
        }

        println(s"Opp. rule head choice: $ohc")
        val newReasoner = state.automaticReasoner.copy(oRuleHeadChoice = ohc)
        updateReasoners(newReasoner)

      case s"prc $x" =>
        val prc = x.toLowerCase match {
          case "bmin" => RuleChoice2.BodyMin
          case "bmax" => RuleChoice2.BodyMax
          case "smin" => RuleChoice2.NewlyIntroducedStatementsMin
          case "smax" => RuleChoice2.NewlyIntroducedStatementsMax
          case "amin" => RuleChoice2.NewlyIntroducedAssumptionsMin
          case "amax" => RuleChoice2.NewlyIntroducedAssumptionsMax
          case "l1s" => RuleChoice2.LookAhead1Step
          case _ =>
            println("Invalid.")
            RuleChoice2.BodyMin
        }
        println(s"Prop. rule choice: $prc")
        val newReasoner = state.automaticReasoner.copy(pRuleChoice = prc)
        updateReasoners(newReasoner)

      case s"orc $x" =>
        val orc = x.toLowerCase match {
          case "bmin" => RuleChoice2.BodyMin
          case "bmax" => RuleChoice2.BodyMax
          case "smin" => RuleChoice2.NewlyIntroducedStatementsMin
          case "smax" => RuleChoice2.NewlyIntroducedStatementsMax
          case "amin" => RuleChoice2.NewlyIntroducedAssumptionsMin
          case "amax" => RuleChoice2.NewlyIntroducedAssumptionsMax
          case "l1s" => RuleChoice2.LookAhead1Step
          case _ =>
            println("Invalid.")
            RuleChoice2.BodyMin
        }
        println(s"Opp. rule choice: $orc")
        val newReasoner = state.automaticReasoner.copy(oRuleChoice = orc)
        updateReasoners(newReasoner)

      case s"order $x" =>
        val possibleMovesSet = Set(PB1, PB2, PF1, PF2, OB1, OB2, OF1, OF2).map(_.toString.toLowerCase())
        val movesSeqString = x.toLowerCase.replaceAll("\\s","").grouped(3).toList

        if (possibleMovesSet != movesSeqString.toSet) {
          println("Invalid ordering. Specify all 8 moves in an order.")
          state
        } else {

          val movesSeq = movesSeqString.map(Move.fromString)
          // make sure all are in there, OF1 should nevertheless be irrelevant
          //val seq = if (movesSeq.contains(OF1)) movesSeq else OF1 +: movesSeq
          println(s"Preference ordering: [${movesSeq.mkString(", ")}]")
          val newReasoner = state.automaticReasoner.copy(preferenceOrdering = movesSeq)
          updateReasoners(newReasoner)
        }

      case "arg" =>
        val fileName = s"arg_dot_repr_step$stateId.dot"
        generateABRepresentation(outputFileName = fileName)
        println(s"Argument-based representation exported to: $fileName")
        state.copy(redraw = false)
      case s"arg $fileName" =>
        generateABRepresentation(outputFileName = fileName)
        println(s"Argument-based representation exported to: $fileName")
        state.copy(redraw = false)
      case s"argp 1" =>
        println(s"Continuous argument representation generation switched ON.")
        state.copy(
          generatedArg = true,
          redraw = false)
      case s"argp 0" =>
        println(s"Continuous argument representation generation switched OFF.")
        state.copy(
          generatedArg = false,
          redraw = false)

      case s"rule" =>
        val fileName = s"rule_dot_repr_step$stateId.dot"
        generateRuleRepresentation(outputFileName = fileName)
        println(s"Rule representation exported to: $fileName")
        state.copy(redraw = false)
      case "?" =>
        println(
          s"+-------------------+\n" +
          s"|  Possible moves:  |\n" +
          s"+-------------------+\n" +
          s"\n" +
          s"${Move.possibleMovesToString(possibleMoves)}\n")
        state.copy(redraw = false)
      case "??" =>
        println(s"Possible moves according to all dispute advancements:\n${Move.possibleMovesAccordingToAllAdvancementToString}\n")
        state
      case "q" | "quit" =>
        state.copy(quit = true)
      case "state" =>
        println(currentDState.toString)
        state
      case "state 0" =>
        state.copy(showState = false)
      case "state 1" =>
        state.copy(showState = true)
      case "h" | "help" =>
        printHelp()
        state
      case "debug" =>
        printDebuggingInformation
        state
      case "s" | "show" =>
        state.copy(redraw = true)
      case "d" =>
        printDecoratorInformation()
        state
      case "i" =>
        println(s"Advancement type: $currentDAdvancement")
        println(s"Termination criteria type: $currentTCriteria")
        state.copy(redraw=false)
      case "info" =>
        // TODO: cleaner
        println("Available advancement types:\n" +
          s"${if (currentDAdvancement == DAB)"*" else ""}\tDAB\n" +
          s"${if (currentDAdvancement == DABF)"*" else ""}\tDABF\n" +
          s"${if (currentDAdvancement == DC)"*" else ""}\tDC\n" +
          s"${if (currentDAdvancement == DS)"*" else ""}\tDS\n" +
          s"${if (currentDAdvancement == DF)"*" else ""}\tDF\n" +
          "Available termination criteria:\n" +
          s"${if (currentTCriteria == TA)"*" else ""}\tTA\n" +
          s"${if (currentTCriteria == TC)"*" else ""}\tTC\n" +
          s"${if (currentTCriteria == TS )"*" else ""}\tTS")
        state.copy(redraw = false)
      case "a" | "assumptions" =>
        print(s"Assumptions: { ${framework.assumptions.mkString("; ")} }\n")
        //println(s"Assumptions: \n\t${framework.decorateAssumptions.map(_._2).mkString("; ")}\n")
        //(derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
        state
      case "r" | "rules" =>
        print(s"Rules: { ${framework.rules.mkString("; ")} }\n")
        //println(s"Rules:\n\t${framework.decorateRules.map(_._2).mkString("; ")}\n")
        state
      case "c" | "contraries" =>
        print(s"Contraries: \n\t${framework.contraries.map(ctr => s"${ctr.assumption}:${ctr.contrary}").mkString("\n\t")}\n")
        state
      case "moves" =>
        // TODO:

        val numberLength = state.performedMoves.length.toString.length
        println(state.performedMoves.zipWithIndex.map { case (arg, index) => s"%0${numberLength}d".format(index + 1) + s": [$arg]" }.mkString("\n"))

        //???
        // TODO
        //println(s"Moves sequence:\n\t${derivation.map(_.sequenceElement).mkString("; ")}\n")
        state
      case "m" | "more" =>
        //???
        printAdditionalInformation
        state
      case s"dotp 1" =>
        println(s"Continuous rule based representation generation switched ON.")
        state.copy(generateDot = true)
      case s"dotp 0" =>
        println(s"Continuous rule based representation generation switched OFF.")
        state.copy(generateDot = false)
      //case s"dot" =>
      //  val fileName = s"rule_repr_step${stateId}.dot"
      //  DotConverter.exportDotRepr(outputFileName = fileName)
//        println(s"Rule based representation exported to: $fileName")
//        state
//      case s"dot s" =>
//        val fileName = s"rule_repr_step$stateId.dot"
//        DotConverter.exportDotRepr(gradientFill = false)
//        println(s"Rule based representation exported to: $fileName")
//        state
//      case s"dot s $fileName" =>
//        DotConverter.exportDotRepr(gradientFill = false, outputFileName = fileName)
//        println(s"Rule based representation exported to: $fileName")
//        state
//      case s"dot $fileName" =>
//        DotConverter.exportDotRepr(outputFileName = fileName)
//        println(s"Rule based representation exported to: $fileName")
//        state
      case s"legend" =>
        val legFileName = DotConverter.exportLegend()
        println(s"DOT legend exported to: $legFileName")
        state

        // TODO: forward using a strategy

//      case "f" =>
//        val (newPerformedMoves, newDState) = forward(currentDState, possibleMoves)
//        state.copy(
//          currentDState = newDState,
//          state.performedMoves ++ newPerformedMoves,
//          state.performedMovesChunks :+ newPerformedMoves
//        )
//
      case s"f $x" if digitRegex.matches(x) =>
        findSuccessfulDerivations2(onlyOne = true, performNMoves = Some(x.toInt))

      case s"f $x" =>
        print(s"Number required, $x passed.")
        state

      case "b" =>

        val (newPerformedMoves, newPerformedMovesChunks, newDState) =
          backward(state.performedMoves, state.performedMovesChunks)

        println(s"Retracting the last move.")


        state.copy(
          performedMoves = newPerformedMoves,
          performedMovesChunks = newPerformedMovesChunks,
          currentDState = newDState,
          redraw = true
          //interactiveOver = state.interactiveReasoner.checkIfOver(state.framework, newDState)
        )
      case "bb" =>

        println(s"Dispute derivation reset to the initial state.")
        state.copy(
          performedMoves = Nil,
          performedMovesChunks = Nil,
          currentDState = DisputeState.initial,
          //interactiveOver = None // TODO:
        )
      case s"b $x" if digitRegex.matches(x) =>
        val (newPerformedMoves, newPerformedMovesChunks, newDState) = backward(state.performedMoves, state.performedMovesChunks, n=x.toInt)

        println(s"Retracting the last $x moves.")

        state.copy(
          performedMoves = newPerformedMoves,
          performedMovesChunks = newPerformedMovesChunks,
          currentDState = newDState,
          redraw = true
          //interactiveOver = state.interactiveReasoner.checkIfOver(state.framework, newDState)
        )

      case "bi" =>
        val (newPerformedMoves, newPerformedMovesChunks, newDState) = backwardChunk(state.performedMovesChunks)

        state.copy(
          performedMoves = newPerformedMoves,
          performedMovesChunks = newPerformedMovesChunks,
          currentDState = newDState,
          //interactiveOver = state.interactiveReasoner.checkIfOver(state.framework, newDState)
        )
      case s"bi $x" if digitRegex.matches(x) =>
        val (newPerformedMoves, newPerformedMovesChunks, newDState) = backwardChunk(state.performedMovesChunks, n=x.toInt)

        state.copy(
          performedMoves = newPerformedMoves,
          performedMovesChunks = newPerformedMovesChunks,
          currentDState = newDState,
          //interactiveOver = state.interactiveReasoner.checkIfOver(state.framework, newDState)
        )
      case s"b $x" =>
        print(s"Number required, $x passed.")
        state
      case s"ct $newCriteria" if terminationCriteriaTypesStrings.contains(newCriteria.toLowerCase) =>
        println(s"Termination criteria set to: ${newCriteria.toUpperCase()}")

        state.copy(
          redraw = false,
          tCriteria = newCriteria,
          automaticReasoner = state.automaticReasoner.copy(tCriteriaType = newCriteria), // TODO: also change other
          approximateReasoner = state.approximateReasoner.copy(
            state.approximateReasoner.automaticReasoner2.copy(tCriteriaType = newCriteria)
          ),
          interactiveReasoner = state.interactiveReasoner.copy(tCriteriaType = newCriteria)
        )

      case s"ct $newCriteria" if !terminationCriteriaTypesStrings.contains(newCriteria.toLowerCase) =>
        println(s"No termination criteria of type: $newCriteria")
        state.copy(redraw = false)
      case s"ca $newAdvancement" if advancementTypesStrings.contains(newAdvancement.toLowerCase) =>
        println(s"Advancement type set to ${newAdvancement.toUpperCase()}")
        state.copy(
          redraw=false,
          dAdvancement = newAdvancement,
          automaticReasoner = state.automaticReasoner.copy(dAdvancementType = newAdvancement), // TODO: also change other
          approximateReasoner = state.approximateReasoner.copy(
            state.approximateReasoner.automaticReasoner2.copy(dAdvancementType = newAdvancement)
          ),
          interactiveReasoner = state.interactiveReasoner.copy(dAdvancementType = newAdvancement)
        )
      case s"ct $newAdvancement" if !terminationCriteriaTypesStrings.contains(newAdvancement.toLowerCase) =>
        println(s"No advancement of type: $newAdvancement")
        state.copy(
          redraw = false
        )
      case s"$move" if possibleMoveTypesStrings.contains(move.toLowerCase()) =>
        val moveToPerform = possibleMoves(move).random
        state.copy(
          performedMoves = state.performedMoves :+ moveToPerform,
          currentDState = moveToPerform.perform,
          redraw = true
        )
      case s"$move $x" if possibleMoveTypesStrings.contains(move.toLowerCase) && digitRegex.matches(x) => // TODO: this regex here
        val movesNo = x.toInt
        val movesOfType = possibleMoves(move) // implicit conversion
        if (movesOfType.size >= movesNo + 1) {
          val moveToPerform = movesOfType(movesNo)
          state.copy(
            performedMoves = state.performedMoves :+ moveToPerform,
            currentDState = moveToPerform.perform,
            redraw = true,
          )
        } else {
          println(s"Wrong index. $move")
          state
        }
      case s"$move" if moveTypesStrings.contains(move.toLowerCase) && !possibleMoveTypesStrings.contains(move.toLowerCase) =>
        println(s"Move $move not currently applicable.")
        state
      case s"$move $_" if moveTypesStrings.contains(move.toLowerCase) && !possibleMoveTypesStrings.contains(move.toLowerCase) =>
        println(s"Move $move not currently applicable.")
        state
      case _ =>
        println("Error")
        state
    }

    // re-calculate new possible moves and termination criteria
    val newPossibleMoves = DisputeAdvancement(newState.dAdvancement).getPossibleMoves(framework, newState.currentDState)
    val newTerminationCriteriaOver = TerminationCriteria.checkIfOver(newState.dAdvancement, newState.tCriteria)(framework, newState.currentDState, newPossibleMoves)

    newState.copy(
      possibleMoves = newPossibleMoves,
      terminationCriteriaOver = newTerminationCriteriaOver
    )
  }

  def printAdditionalInformation(implicit dState: DisputeState, framework: Framework): Unit = {
    //    val nonDefencesNonCulpritsRemainingAssumptions = (framework.assumptions -- framework.defences -- framework.culprits).toSeq.sortBy(_.id)
    //    println(s"Non-defences, non-culprits remaining assumptions:\n\t${nonDefencesNonCulpritsRemainingAssumptions.mkString("; ")}")
    //
    //    val notPlayedAssumptions = nonDefencesNonCulpritsRemainingAssumptions.filterNot(ass => dState.bLitArgs.exists(_.lit == ass)).sortBy(_.id)
    //    println(s"Non-played, non-culprits assumptions:\n\t${notPlayedAssumptions.mkString("; ")}")
    //
    //    // currently defended assumptions not in defenses
    //    val currentlyDefendedAssumptionsNotInDefences = (framework.j -- framework.defences).toSeq.sortBy(_.id)
    //    println(s"Currently defended assumptions not in defences:\n\t${currentlyDefendedAssumptionsNotInDefences.mkString("; ")}")
  }

  def printDebuggingInformation(implicit dState: DisputeState, framework: Framework): Unit = {


    //    val culpritCandidates = framework.culpritsCandidates
    //    val defences = framework.defences
    //
    //    println("======================\n" +
    //            "        GENERAL\n" +
    //            "======================\n")
    //    Seq(
    //      ("Culprit candidates", culpritCandidates),
    //      ("Culprit candidates contraries", framework.contrariesOf(culpritCandidates)),
    //      ("Remaining non-blocked rules for proponent", framework.remainingNonBlockedPRules),
    //      ("Unexpanded prop statements", framework.unexpandedPStatements),
    //      ("Defences", defences),
    //      ("Defence contraries", framework.contrariesOf(defences)),
    //      ("Complete prop arguments", framework.completePiecesP),
    //      ("J (for complete sem.)", framework.j),
    //      ("Unexpanded statements for the proponent", framework.unexpandedPStatements),
    //      ("Played fully expanded statements", framework.fullyExpandedStatements),
    //      ("Played blocked pieces", framework.playedBlockedPieces),
    //      ("Unblocked complete played pieces of the opponent", framework.unblockedCompletePlayedPiecesB),
    //    ).foreach{ case (desc, set) => println(s"$desc:\n\t${set.mkString("; ")}") }
    //
    //    println("======================\n" +
    //            " TERMINATION CRITERIA\n" +
    //            "======================\n")
    //
    //    TerminationCriteria.seemsToBeWinningDebug
  }

  def printHelp(): Unit = {
    println("Program options:")
    println("\t ?\t- print possible moves.")
    println("\t ??\t- print possible moves according to all advancement types.")
    println("\t h | help\t- print this help information.")
    println("\t s | show\t- print dispute state.")
    println("\t q | quit\t- quit.")
    println("\t i | info\t- print information about selected advancement type and termination criteria.")
    println("\t a\t- print assumptions.")
    println("\t r\t- print rules.")
    println("\t m | more\t- print more information.")
    println("\t moves\t- print moves sequence.")
    println("\t d\t- print info about decorators.")
    println("\t dot [s] [filename]\t- export current dispute to a graph DOT file. Optionally:" +
      "\n\t\ts - solid fill colours." +
      "\n\t\tfilename - specify output filename.")
    println("\t legend\t- export legend explaining used colours / shapes to a DOT file.")
    println("\t f [n] - perform random move forward. Optionally:" +
      "\n\t\tn - perform n random moves forward.")
    println("\t b [n] - backward 1 move. Optionally:" +
      "\n\t\tn - backward n moves.")
    println("\t ct <T> - set termination criteria to <T>. Possible termination criteria are:" +
      "\n\t\tTA, TC, TS (case insensitive)")
    println("\t ca <A> - set advancement type to <A>. Possible advancement types are:" +
      "\n\t\tDAB, DABF, DC, DS, DF (case insensitive)")
    println("\t <MOVE> [i] - perform move of type <MOVE> (if possible). Optionally:" +
      "\n\t\tPerform move of index i when viewing possible moves with \"?\"" +
      "\n\t\tPossible move types are: {P,O}x{B,F}x{1,2}, i.e. PB1, PB2, PF1, PF2, OB1, OB2, OF1, OF2 (case insensitive)")
  }

  def printDecoratorInformation(): Unit = {
    println("Played pieces:")
    println("\t $\t- proponent piece.")
    println("\t *\t- complete piece.")
    println("\t **\t- complete piece for proponent.")
    println("\t \"\t- unexpanded statements of the proponent.")
    println("\t ^\t- assumptions and fully expanded statements.")
    println("\t --\t- played blocked pieces.")
    println("\t !\t- opponent assumptions, contraries of defences.\n")

    println("Assumptions:")
    println("\t @\t- assumption used by opponent.")
    println("\t &\t- assumption used by proponent.")
    println("\t ~\t- blocked assumptions.")
    println("\t --\t- culprits.\n")

    println("Rules:")
    println("\t @\t- rule used by opponent.")
    println("\t &\t- rule used by proponent.")
    println("\t ~\t- Blocked rule because of inconsistency of constraints or contraries of defences in head or body.")
    println("\t --\t- Blocked rules because of culprits in bodies.")
  }

  @tailrec
  private def forward(dState: DisputeState,
                      possibleMoves: Map[MoveType, Seq[PotentialMove2]],
                      performedMoves: List[PotentialMove2] = Nil, n: Int = 1)
                     (implicit framework: Framework,
                      dAdvancement: DisputeAdvancementType,
                      tCriteria: TerminationCriteriaType
                     ): (List[PotentialMove2], DisputeState) = {
    //implicit val lastState: DisputeState = derivation.last
    //implicit lazy val possibleMoves: Map[MoveType, Seq[PotentialMove2]] = DisputeAdvancement(dAdvancement).getPossibleMoves // Move.getPossibleMoves  // lazy val to evaluate it only when n > 0

    TerminationCriteria.checkIfOver(dAdvancement, tCriteria)(framework, dState, possibleMoves) match {
      case Some(_) => return (performedMoves, dState)
      case _ =>
    }

    println(s"$n moves left.")

    if (n == 0 || possibleMoves.isEmpty) (performedMoves, dState)  // TODO: add checks for won game
    else {
      val moveToPerform = possibleMoves.randomElement
      val newDState = moveToPerform.perform(dState, framework)

      val newPossibleMoves = DisputeAdvancement(dAdvancement).getPossibleMoves(framework, newDState)
      forward(newDState, newPossibleMoves, performedMoves :+ moveToPerform, n - 1)
    }
  }


  private def backwardChunk(performedMovesChunks: List[List[PotentialMove2]], n: Int = 1)
                           (implicit framework: Framework): (List[PotentialMove2], List[List[PotentialMove2]], DisputeState) = {

    val newPerformedMovesChunks = performedMovesChunks.dropRight(n)
    val newPerformedMoves = newPerformedMovesChunks.flatten

    val initialDState = DisputeState.initial
    val newDState = reconstructStateFromMoves(initialDState, newPerformedMoves)

    (newPerformedMoves, newPerformedMovesChunks, newDState)

  }



  private def backward(performedMoves: List[PotentialMove2],
                       performedMovesChunks: List[List[PotentialMove2]],
                       n: Int = 1)(implicit framework: Framework): (List[PotentialMove2], List[List[PotentialMove2]], DisputeState) = {

    val newPerformedMoves = performedMoves.dropRight(n)
    val newPerformedMovesCount = newPerformedMoves.size

    val newPerformedMovesChunks = performedMovesChunks.foldLeft(Nil: List[List[PotentialMove2]])((currChunks, newChunk) => {
      if (currChunks.flatten.size == newPerformedMovesCount) currChunks // it has already been done
      else {
        val newChunksSize = (currChunks :+ newChunk).flatten.size
        if (newChunksSize < newPerformedMovesCount) currChunks :+ newChunk // append new chunk
        else {
          val remainingChunkSize = newPerformedMovesCount - currChunks.flatten.size
          val lastChunk = newChunk.take(remainingChunkSize) // append only a part of the chunk
          currChunks :+ lastChunk
        }
      }
    })

    val initialDState = DisputeState.initial
    val newDState = reconstructStateFromMoves(initialDState, newPerformedMoves)

    (newPerformedMoves, newPerformedMovesChunks, newDState)
  }

  private def reconstructStateFromMoves(initialDState: DisputeState, performedMoves: List[PotentialMove2])
                                       (implicit framework: Framework):  DisputeState = {

    performedMoves.foldLeft(initialDState) { case (state, move) =>
      move.perform(state, framework)
    }
  }





}
