import scala.language.implicitConversions
import aba.framework.Framework
import aba.move.DisputeAdvancement.{DAB, DisputeAdvancementType}
import aba.move.{DisputeAdvancement, Move, TerminationCriteria}
import aba.move.Move.MoveType
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.argumentBased.DisputeStateAB_
import aba.reasoner.argumentBased2.{ArgumentTree, DisputeStateAB2}
import aba.reasoner.automatic.{AssumptionChoice, AttackPreference, AutoPreferenceReasoner, AutomaticReasoner, DisputeStateAuto, OpponentsAttackStrategy, RuleChoice, StatementChoice, TurnChoice}
import aba.reasoner.{DisputeState, PotentialMove}
import commandLineParser.CommandLineParser
import dot.{ABDotConverter, DotConverter}

import java.io.PrintWriter
import scala.annotation.tailrec

// TODO: add functionality to add the goals dynamically in code OR via commandline args

object Main {

  def main(args: Array[String]): Unit = {

    CommandLineParser.parse(args) match {
      case Some(config) =>
        implicit val framework: Framework = Framework(config.inputFormat, config.inputFilePath)
        println("\n\n==============\nDerivation started.\n==============\n")

        implicit val initialDState: DisputeState = framework.initialDState

        val defaultDAdvancement = DAB
        val defaultTCriteria = TA
        disputeDerivation(initialDState :: Nil, redraw = true, currentGenDot = false, currentGenArg = false, defaultDAdvancement, defaultTCriteria)
      case _ =>
    }

  }

  // find one for each strategy
  def findAll(dAdvancement: DisputeAdvancementType,
              tCriteria: TerminationCriteriaType,
              dfs: Boolean)(implicit framework: Framework,
                            possibleMoves: Map[MoveType, Seq[PotentialMove]],
                            lastState: DisputeState): Unit = {

    val folderName = "auto_find_one_by_all"

    var i = 1
    for { turnChoice <- TurnChoice.values
      pStatementChoice <- StatementChoice.values
      oStatementChoice <- StatementChoice.values
      oppAttackStrategy = OpponentsAttackStrategy.OneAtATime
      pAttackPreference <- AttackPreference.values
      oAttackPreference <- AttackPreference.values
      pRuleChoice <- RuleChoice.values
      oRuleChoice <- RuleChoice.values
      pAssumptionChoice <- AssumptionChoice.values
      oAssumptionChoice <- AssumptionChoice.values
    } {


        implicit val automaticReasoner: AutomaticReasoner = new AutomaticReasoner(
          turnChoice = turnChoice,
          pStatementChoice = pStatementChoice,
          oStatementChoice = oStatementChoice,
          opponentsAttackStrategy = OpponentsAttackStrategy.OneAtATime,
          pAttackPreference = pAttackPreference,
          oAttackPreference = oAttackPreference,
          pRuleChoice = pRuleChoice,
          oRuleChoice = oRuleChoice,
          pAssumptionChoice = pAssumptionChoice,
          oAssumptionChoice = oAssumptionChoice,
          dfs = dfs
        )


        val initialDStateAuto = new DisputeStateAuto(lastState, Set.empty, Set.empty, Nil)
        val initialDSs = List(initialDStateAuto)

        val chosenStrategyString = "// "+  automaticReasoner.settingsToString.mkString("\n// ")

        val (info, filename) = automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(initialDSs, Nil)(framework, onlyOne = true, Some(60), tCriteria, dAdvancement) match {
          case (_, _, true, duration) =>
            println(s"$i: Timeout")
            (s"// Timeout reached (computation time $duration)", s"timeout_$i.txt")
          case (Nil, Nil, _, duration) =>
            println(s"$i: Not found")
            (s"// No successful derivations found (computation time $duration)", s"not_found_$i.txt")
          case (_, successfulHead :: _, _, duration) =>
            println(s"$i: Success")
            (s"// Successful derivation found (computation time $duration).\n// " +
            successfulHead.performedMovesToString.mkString("\n// "), s"success_$i.txt")
        }

      new PrintWriter(s"$folderName/$filename") { write(chosenStrategyString + "\n" + info); close() }

      i += 1
    }
  }

  @tailrec
  def disputeDerivation(derivation: List[DisputeState], redraw: Boolean, currentGenDot: Boolean, currentGenArg: Boolean,
                        dAdvancementType: DisputeAdvancementType, tCriteriaType: TerminationCriteriaType)
                       (implicit framework: Framework): List[DisputeState] = {

    implicit val lastState: DisputeState = derivation.last   // get last derivation state

    if (redraw) {
      println(s"««« ${lastState.sequenceElement} »»»")

      println(s"${framework.disputeStateToString()}\n")

    }

    implicit val possibleMoves: Map[MoveType, Seq[PotentialMove]] = DisputeAdvancement(dAdvancementType).getPossibleMoves

    TerminationCriteria.checkIfOver(tCriteriaType) match {
      case Some(propWon) => println(s"Game over. ${ if (propWon) "Proponent" else "Opponent" } won.")
      case _ =>
    }

    if (currentGenArg) generateABRepresentation()
    if (currentGenDot) DotConverter.exportDotRepr()

    val (newDerivation, stop, nextRedraw, nextGenDot, nextGenArg, nextAdvancementType, nextTerminationType) = progressDerivation(derivation, currentGenDot, currentGenArg, dAdvancementType, tCriteriaType)

    if (!stop) disputeDerivation(newDerivation, nextRedraw, nextGenDot, nextGenArg, nextAdvancementType, nextTerminationType)
    else newDerivation
  }

  private def generateABRepresentation(showCircular: Boolean=true, showIncomplete: Boolean=true, outputName: String = "temp_arg.dot", additionalInformation: String = "")(implicit dState: DisputeState, framework: Framework): String = {
    val actualPropGoals = framework.goals union framework.contrariesOf(framework.culprits)

    def filterArgs: ArgumentTree => Boolean = arg => (showCircular, showIncomplete) match {
      case (true, true) => true
      case (true, false) => arg.isComplete
      case (false, true) => !arg.isCircular
      case (false, false) => !arg.isCircular && arg.isComplete
    }

    val propArgs = DisputeStateAB2.create_arguments(actualPropGoals, dState.pRuleArgs.map(_.rule)).filter(filterArgs)

    val opponentGoals = framework.contrariesOf(framework.defences) intersect dState.bLitArgs.map(_.lit) // those that actually have been uttered
    val oppArgs = DisputeStateAB2.create_arguments(opponentGoals, dState.bRuleArgs.map(_.rule)).filter(filterArgs)
    ABDotConverter.exportDotRepr(propArgs, oppArgs, outputName, additionalInformation)

    outputName
  }

  private def findSuccessfulDerivations(
                                  dAdvancement: DisputeAdvancementType,
                                  tCriteria: TerminationCriteriaType,
                                  dfs: Boolean)(implicit framework: Framework,
                                                //automaticReasoner: AutomaticReasoner,
                                                possibleMoves: Map[MoveType, Seq[PotentialMove]],
                                                lastState: DisputeState): DisputeState = {

    println("Finding a successful derivation. This can take a moment...")

    implicit val automaticReasoner: AutomaticReasoner = new AutomaticReasoner(
      turnChoice = TurnChoice.Proponent,
      pStatementChoice = StatementChoice.Eager,
      oStatementChoice = StatementChoice.Eager,
      opponentsAttackStrategy = OpponentsAttackStrategy.OneAtATime,
      pAttackPreference = AttackPreference.PreferRuleAttack,
      oAttackPreference = AttackPreference.PreferRuleAttack,
      pRuleChoice = RuleChoice.NewlyIntroducedAssumptionsMin,
      oRuleChoice = RuleChoice.NewlyIntroducedAssumptionsMin,
      pAssumptionChoice = AssumptionChoice.MostContraries,
      oAssumptionChoice = AssumptionChoice.MostContraries,
      dfs = dfs
    )

    val initialDStateAuto = new DisputeStateAuto(lastState, Set.empty, Set.empty, Nil)
    val initialDSs = List(initialDStateAuto)

    @tailrec
    def findSuccessfulDerivationsRec(stack: List[DisputeStateAuto]): DisputeState = {

      automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(stack, Nil)(framework, onlyOne = true, None, tCriteria, dAdvancement) match {
        case (_, _, true, _) =>
          println("Timeout reached")
          lastState
        case (Nil, Nil, _, _) =>
          println("No successful derivations found")
          lastState
        case (restDS, successfulHead :: _, _, duration) =>
          println(s"Successful derivation found in $duration.")
          println(successfulHead.performedMovesToString.mkString("\n"))
          println("Press ENTER to finish, ; to find another one")
          Console.in.readLine match {
            case ";" => findSuccessfulDerivationsRec(restDS)
            case _ => successfulHead.dState
          }
      }
    }

    findSuccessfulDerivationsRec(initialDSs)
  }


  private def findSuccessfulDerivationsPref(dAdvancement: DisputeAdvancementType,
                                         tCriteria: TerminationCriteriaType,
                                         dfs: Boolean)(implicit framework: Framework,
                                                       //automaticReasoner: AutomaticReasoner,
                                                       possibleMoves: Map[MoveType, Seq[PotentialMove]],
                                                       lastState: DisputeState): DisputeState = {

    println("Finding a successful derivation. This can take a moment...")

    implicit val automaticReasoner: AutoPreferenceReasoner = new AutoPreferenceReasoner(dfs)


    val initialDStateAuto = new DisputeStateAuto(lastState, Set.empty, Set.empty, Nil)
    val initialDSs = List(initialDStateAuto)

    @tailrec
    def findSuccessfulDerivationsRec(stack: List[DisputeStateAuto]): DisputeState = {

      automaticReasoner.getNewIncompleteSuccessfulDSAndStackRec(stack, Nil)(framework, onlyOne = true, None, tCriteria, dAdvancement) match {
        case (_, _, true, _) =>
          println("Timeout reached")
          lastState
        case (Nil, Nil, _, _) =>
          println("No successful derivations found")
          lastState
        case (restDS, successfulHead :: _, _, duration) =>
          println(s"Successful derivation found in $duration.")
          println(successfulHead.performedMovesToString.mkString("\n"))
          println("Press ENTER to finish, ; to find another one")
          Console.in.readLine match {
            case ";" => findSuccessfulDerivationsRec(restDS)
            case _ => successfulHead.dState
          }
      }
    }

    findSuccessfulDerivationsRec(initialDSs)
  }

  def progressDerivation(derivation: List[DisputeState],
                         genDot: Boolean,
                         genArg: Boolean,
                         dAdvancement: DisputeAdvancementType,
                         tCriteria: TerminationCriteriaType)(implicit framework: Framework,
                                                         possibleMoves: Map[MoveType, Seq[PotentialMove]],
                                                         lastState: DisputeState):
  (List[DisputeState],
    Boolean, // stop
    Boolean, // nextRedraw
    Boolean, // nextGenDot
    Boolean, // nextGenArg
    DisputeAdvancementType, TerminationCriteriaType) = {

    val digitRegex = """\d+""".r

    val Seq(
      moveTypesStrings,
      advancementTypesStrings,
      terminationCriteriaTypesStrings
    )
    = Seq(Move, DisputeAdvancement, TerminationCriteria).map(_.values.map(_.toString.toLowerCase()))

    val possibleMoveTypesStrings = possibleMoves.keys.map(_.toString.toLowerCase()).toSet

    Console.in.readLine match {
      case "auto dfs all" =>
        findAll(dAdvancement, tCriteria, dfs=true)
        (derivation, false,  false, genDot, genArg, dAdvancement, tCriteria)
      case s"auto1 dfs" =>
        val newDerivation = findSuccessfulDerivationsPref(dAdvancement, tCriteria, dfs=true)
        // TODO: now you cannot backtrack from here
        (derivation :+ newDerivation, false,  false, genDot, genArg, dAdvancement, tCriteria)
      case s"auto1 bfs" =>
        val newDerivation = findSuccessfulDerivationsPref(dAdvancement, tCriteria, dfs=false)
        // TODO: now you cannot backtrack from here
        (derivation :+ newDerivation, false,  false, genDot, genArg, dAdvancement, tCriteria)
      case s"auto dfs" =>
        val newDerivation = findSuccessfulDerivations(dAdvancement, tCriteria, dfs=true)
        // TODO: now you cannot backtrack from here
        (derivation :+ newDerivation, false,  false, genDot, genArg, dAdvancement, tCriteria)
      case s"auto bfs" =>
        val newDerivation = findSuccessfulDerivations(dAdvancement, tCriteria, dfs=false)
        // TODO: same as above. It is possible to convert the new derivation AUTO to a list of dispute states, do that
        //  OR do not keep the lists of Dstates but generate them from the sequence of moves
        (derivation :+ newDerivation, false,  false, genDot, genArg, dAdvancement, tCriteria)
      case "arg" =>
        val fileName = s"arg_dot_repr_step${lastState.id}.dot"
        generateABRepresentation(outputName = fileName)
        println(s"Argument-based representation exported to: $fileName")
        (derivation, false,  false, genDot, genArg, dAdvancement, tCriteria)
      case s"arg $fileName" =>
        generateABRepresentation(outputName = fileName)
        println(s"Argument-based representation exported to: $fileName")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"argp 1" =>
        println(s"Continuous argument representation generation switched ON.")
        (derivation, false, false, genDot, true, dAdvancement, tCriteria)
      case s"argp 0" =>
        println(s"Continuous argument representation generation switched OFF.")
        (derivation, false, false, genDot, false, dAdvancement, tCriteria)
      case "?" =>
        println(s"Possible moves:\n${Move.possibleMovesToString(possibleMoves)}\n")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "??" =>
        println(s"Possible moves according to all dispute advancements:\n${Move.possibleMovesAccordingToAllAdvancementToString}\n")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "q" | "quit" =>
        (derivation, true, false, genDot, genArg, dAdvancement, tCriteria)
      case "h" | "help" =>
        printHelp()
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "debug" =>
        printDebuggingInformation
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "s" | "show" =>
        (derivation, false, true, genDot, genArg, dAdvancement, tCriteria)
      case "d" =>
        printDecoratorInformation()
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "i" | "info" =>
        println(s"Advancement type: $dAdvancement")
        println(s"Termination criteria type: $tCriteria")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "a" | "assumptions" =>
        // TODO
        //println(s"Assumptions: \n\t${framework.decorateAssumptions.map(_._2).mkString("; ")}\n")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "r" | "rules" =>
        // TODO
        //println(s"Rules:\n\t${framework.decorateRules.map(_._2).mkString("; ")}\n")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "moves" =>
        // TODO
        //println(s"Moves sequence:\n\t${derivation.map(_.sequenceElement).mkString("; ")}\n")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "m" | "more" =>
        printAdditionalInformation
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"dotp 1" =>
        println(s"Continuous rule based representation generation switched ON.")
        (derivation, false, false, true, genArg, dAdvancement, tCriteria)
      case s"dotp 0" =>
        println(s"Continuous rule based representation generation switched OFF.")
        (derivation, false, false, false, genArg, dAdvancement, tCriteria)
      case s"dot" =>
        val fileName = s"rule_repr_step${lastState.id}.dot"
        DotConverter.exportDotRepr(outputFileName = fileName)
        println(s"Rule based representation exported to: $fileName")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"dot s" =>
        val fileName = s"rule_repr_step${lastState.id}.dot"
        DotConverter.exportDotRepr(gradientFill = false)
        println(s"Rule based representation exported to: $fileName")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"dot s $fileName" =>
        DotConverter.exportDotRepr(gradientFill = false, outputFileName = fileName)
        println(s"Rule based representation exported to: $fileName")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"dot $fileName" =>
        DotConverter.exportDotRepr(outputFileName = fileName)
        println(s"Rule based representation exported to: $fileName")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"legend" =>
        val legFileName = DotConverter.exportLegend()
        println(s"DOT legend exported to: $legFileName")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "arg" =>
        val dStateAB = DisputeStateAB_(derivation)
        println(s"Dispute state (Argument-Based):\n\t${framework.disputeStateToString(Some(dStateAB.decorateArguments.map(_._2).mkString("; ")))}\n")
        println(s"Rules:\n\t${dStateAB.decorateRules.map(_._2).mkString("; ")}\n")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case "f" =>
        (forward(derivation, dAdvancement, tCriteria), false, true, genDot, genArg, dAdvancement, tCriteria)
      case s"f $x" if digitRegex.matches(x) =>
        (forward(derivation, dAdvancement, tCriteria, x.toInt), false, true, genDot, genArg, dAdvancement, tCriteria)  // TODO: regexes ?
      case s"f $x" =>
        print(s"Number required, $x passed.")
        (derivation, false, true, genDot, genArg, dAdvancement, tCriteria)  // TODO:
      case "b" =>
        (derivation.safeBacktrack(), false, true, genDot, genArg, dAdvancement, tCriteria)
      case "bb" =>
        (derivation.take(1), false, true, genDot, genArg, dAdvancement, tCriteria)
      case s"b $x" if digitRegex.matches(x) =>
        (derivation.safeBacktrack(x.toInt), false, true, genDot, genArg, dAdvancement, tCriteria)  // TODO: regexes ?
      case s"b $x" =>
        print(s"Number required, $x passed.")
        (derivation, false, true, genDot, genArg, dAdvancement, tCriteria)
      case s"ct $newCriteria" if terminationCriteriaTypesStrings.contains(newCriteria.toLowerCase) =>
        println(s"Termination criteria set to: ${newCriteria.toUpperCase()}")
        (derivation, false, false, genDot, genArg, dAdvancement, newCriteria) // implicit conversion
      case s"ct $newCriteria" if !terminationCriteriaTypesStrings.contains(newCriteria.toLowerCase) =>
        println(s"No termination criteria of type: $newCriteria")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"ca $newAdvancement" if advancementTypesStrings.contains(newAdvancement.toLowerCase) =>
        println(s"Advancement type set to ${newAdvancement.toUpperCase()}")
        (derivation, false, false, genDot, genArg, newAdvancement, tCriteria)
      case s"ct $newAdvancement" if !terminationCriteriaTypesStrings.contains(newAdvancement.toLowerCase) =>
        println(s"No advancement of type: $newAdvancement")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"$move" if possibleMoveTypesStrings.contains(move.toLowerCase()) =>
        (derivation :+ possibleMoves(move).random.perform, false, true, genDot, genArg, dAdvancement, tCriteria) // implicit conversion
      case s"$move $x" if possibleMoveTypesStrings.contains(move.toLowerCase) && digitRegex.matches(x) =>
        val movesNo = x.toInt
        val movesOfType = possibleMoves(move) // implicit conversion
        if (movesOfType.size >= movesNo + 1) {
          (derivation :+ movesOfType(movesNo).perform, false, true, genDot, genArg, dAdvancement, tCriteria)
        } else {
          println(s"Wrong index. $move")
          (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
        }
      case s"$move" if moveTypesStrings.contains(move.toLowerCase) && !possibleMoveTypesStrings.contains(move.toLowerCase) =>
        println(s"Move $move not currently applicable.")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
      case s"$move $_" if moveTypesStrings.contains(move.toLowerCase) && !possibleMoveTypesStrings.contains(move.toLowerCase) =>
        println(s"Move $move not currently applicable.")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)

      case _ =>
        println("Error")
        (derivation, false, false, genDot, genArg, dAdvancement, tCriteria)
    }
  }

  def printAdditionalInformation(implicit dState: DisputeState, framework: Framework): Unit = {
    val nonDefencesNonCulpritsRemainingAssumptions = (framework.assumptions -- framework.defences -- framework.culprits).toSeq.sortBy(_.id)
    println(s"Non-defences, non-culprits remaining assumptions:\n\t${nonDefencesNonCulpritsRemainingAssumptions.mkString("; ")}")

    val notPlayedAssumptions = nonDefencesNonCulpritsRemainingAssumptions.filterNot(ass => dState.bLitArgs.exists(_.lit == ass)).sortBy(_.id)
    println(s"Non-played, non-culprits assumptions:\n\t${notPlayedAssumptions.mkString("; ")}")

    // currently defended assumptions not in defenses
    val currentlyDefendedAssumptionsNotInDefences = (framework.j -- framework.defences).toSeq.sortBy(_.id)
    println(s"Currently defended assumptions not in defences:\n\t${currentlyDefendedAssumptionsNotInDefences.mkString("; ")}")
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
  private def forward(derivation: List[DisputeState],
                      dAdvancement: DisputeAdvancementType,
                      tCriteria: TerminationCriteriaType,
                      n: Int = 1)
                     (implicit framework: Framework): List[DisputeState] = {
    implicit val lastState: DisputeState = derivation.last
    implicit lazy val possibleMoves: Map[MoveType, Seq[PotentialMove]] = DisputeAdvancement(dAdvancement).getPossibleMoves // Move.getPossibleMoves  // lazy val to evaluate it only when n > 0

    TerminationCriteria.checkIfOver(tCriteria) match {
      case Some(_) => return derivation
      case _ =>
    }

    println(s"$n moves left.")

    if (n == 0 || possibleMoves.isEmpty) derivation  // TODO: add checks for won game
    else {
      val newDState = possibleMoves.randomElement.perform
      forward(derivation :+ newDState, dAdvancement, tCriteria, n-1) // tail recursion
    }
  }

  // TODO: move to implicits
  implicit class mapWrapper[K,V](map: Map[K, Seq[V]]) {
    def randomElement: V = {
      val n = util.Random.nextInt(map.size)
      val seq = map.iterator.drop(n).next._2 // entry [MoveType, Seq[PotentialMoves]] -> now take random PotentialMove
      val m = util.Random.nextInt(seq.size)
      seq.iterator.drop(m).next
    }
  }

  implicit class listWrapper[T](list: List[T]) {
    def safeBacktrack(n: Int = 1): List[T] = list.take(math.max(list.size-n, 1))
  }


  implicit class seqWrapper[T](seq: Seq[T]) {
    def random: T = {
      val n = util.Random.nextInt(seq.size)
      seq.iterator.drop(n).next
    }
  }


}
