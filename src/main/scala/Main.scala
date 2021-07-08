import scala.language.implicitConversions

import aba.framework.Framework
import aba.move.DisputeAdvancement.{DAB, DF, DisputeAdvancementType}
import aba.move.{DisputeAdvancement, Move, TerminationCriteria}
import aba.move.Move.MoveType
import aba.move.TerminationCriteria.{TA, TerminationCriteriaType}
import aba.reasoner.argumentBased.DisputeStateAB_
import aba.reasoner.{DisputeState, PotentialMove}
import commandLineParser.CommandLineParser
import dot.DotConverter

import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {

    CommandLineParser.parse(args) match {
      case Some(config) =>
        implicit val framework: Framework = Framework(config.inputFormat, config.inputFilePath)
        println("\n\n==============\nDerivation started.\n==============\n")

        implicit val initialDState: DisputeState = framework.initialDState

        val defaultDAdvancement = DAB
        val defaultTCriteria = TA
        disputeDerivation(initialDState :: Nil, redraw = true, defaultDAdvancement, defaultTCriteria)
        disputeDerivation(initialDState :: Nil, redraw = true, defaultDAdvancement, defaultTCriteria)
      case _ =>
    }

  }

  @tailrec
  def disputeDerivation(derivation: List[DisputeState], redraw: Boolean, dAdvancementType: DisputeAdvancementType, tCriteriaType: TerminationCriteriaType)
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

    val (newDerivation, stop, nextRedraw, nextAdvancementType, nextTerminationType) = progressDerivation(derivation, dAdvancementType, tCriteriaType)

    if (!stop) disputeDerivation(newDerivation, nextRedraw, nextAdvancementType, nextTerminationType)
    else newDerivation
  }

  def progressDerivation(derivation: List[DisputeState],
                         dAdvancement: DisputeAdvancementType,
                         tCriteria: TerminationCriteriaType)(implicit framework: Framework,
                                                         possibleMoves: Map[MoveType, Seq[PotentialMove]],
                                                         lastState: DisputeState):
  (List[DisputeState], Boolean, Boolean, DisputeAdvancementType, TerminationCriteriaType) = {

    val digitRegex = """\d+""".r

    val Seq(
      moveTypesStrings,
      advancementTypesStrings,
      terminationCriteriaTypesStrings
    )
    = Seq(Move, DisputeAdvancement, TerminationCriteria).map(_.values.map(_.toString.toLowerCase()))

    val possibleMoveTypesStrings = possibleMoves.keys.map(_.toString.toLowerCase()).toSet

    Console.in.readLine match {
      case "?" =>
        println(s"Possible moves:\n${Move.possibleMovesToString(possibleMoves)}\n")
        (derivation, false, false, dAdvancement, tCriteria)
      case "??" =>
        println(s"Possible moves according to all dispute advancements:\n${Move.possibleMovesAccordingToAllAdvancementToString}\n")
        (derivation, false, false, dAdvancement, tCriteria)
      case "debug" =>
        printDebuggingInformation
        (derivation, false, false, dAdvancement, tCriteria)
      case "s" | "show" => (derivation, false, true, dAdvancement, tCriteria)
      case "i" | "info" =>
        println(s"Advancement type: $dAdvancement")
        println(s"Termination criteria type: $tCriteria")
        (derivation, false, false, dAdvancement, tCriteria)
      case "a" | "assumptions" =>
        println(s"Assumptions: \n\t${framework.decorateAssumptions.map(_._2).mkString("; ")}\n")
        (derivation, false, false, dAdvancement, tCriteria)
      case "r" | "rules" =>
        println(s"Rules:\n\t${framework.decorateRules.map(_._2).mkString("; ")}\n")
        (derivation, false, false, dAdvancement, tCriteria)
      case "moves" =>
        println(s"Moves sequence:\n\t${derivation.map(_.sequenceElement).mkString("; ")}\n")
        (derivation, false, false, dAdvancement, tCriteria)
      case "m" | "more" =>
        //
        printAdditionalInformation
        (derivation, false, false, dAdvancement, tCriteria)
      case s"dot" =>
        val fileName = DotConverter.exportDotRepr()
        println(s"Graph representation exported to: $fileName")
        (derivation, false, false, dAdvancement, tCriteria)
      case s"dot $fileName" =>
        println(s"Graph representation exported to: $fileName")
        (derivation, false, false, dAdvancement, tCriteria)
      case "arg" =>
        val dStateAB = DisputeStateAB_(derivation)
        println(s"Dispute state (Argument-Based):\n\t${framework.disputeStateToString(Some(dStateAB.decorateArguments.map(_._2).mkString("; ")))}\n")
        println(s"Rules:\n\t${dStateAB.decorateRules.map(_._2).mkString("; ")}\n")
        (derivation, false, false, dAdvancement, tCriteria)
      case "f" =>
        (forward(derivation, dAdvancement, tCriteria), false, true, dAdvancement, tCriteria)
      case s"f $x" if digitRegex.matches(x) =>
        (forward(derivation, dAdvancement, tCriteria, x.toInt), false, true, dAdvancement, tCriteria)  // TODO: regexes ?
      case s"f $x" =>
        print(s"Number required, $x passed.")
        (derivation, false, true, dAdvancement, tCriteria)  // TODO:
      case "b" =>
        (derivation.safeBacktrack(), false, true, dAdvancement, tCriteria)
      case "bb" =>
        (derivation.take(1), false, true, dAdvancement, tCriteria)
      case s"b $x" if digitRegex.matches(x) =>
        (derivation.safeBacktrack(x.toInt), false, true, dAdvancement, tCriteria)  // TODO: regexes ?
      case s"b $x" =>
        print(s"Number required, $x passed.")
        (derivation, false, true, dAdvancement, tCriteria)
      case "q" =>
        (derivation, true, false, dAdvancement, tCriteria)
      case s"ct $newCriteria" if terminationCriteriaTypesStrings.contains(newCriteria.toLowerCase) =>
        println(s"Termination criteria set to: ${newCriteria.toUpperCase()}")
        (derivation, false, false, dAdvancement, newCriteria) // implicit conversion
      case s"ct $newCriteria" if !terminationCriteriaTypesStrings.contains(newCriteria.toLowerCase) =>
        println(s"No termination criteria of type: $newCriteria")
        (derivation, false, false, dAdvancement, tCriteria)
      case s"ca $newAdvancement" if advancementTypesStrings.contains(newAdvancement.toLowerCase) =>
        println(s"Advancement type set to ${newAdvancement.toUpperCase()}")
        (derivation, false, false, newAdvancement, tCriteria)
      case s"ct $newAdvancement" if !terminationCriteriaTypesStrings.contains(newAdvancement.toLowerCase) =>
        println(s"No advancement of type: $newAdvancement")
        (derivation, false, false, dAdvancement, tCriteria)
      case s"$move" if possibleMoveTypesStrings.contains(move.toLowerCase()) =>
        (derivation :+ possibleMoves(move).random.perform, false, true, dAdvancement, tCriteria) // implicit conversion
      case s"$move $x" if possibleMoveTypesStrings.contains(move.toLowerCase) && digitRegex.matches(x) =>
        val movesNo = x.toInt
        val movesOfType = possibleMoves(move) // implicit conversion
        if (movesOfType.size >= movesNo + 1) {
          (derivation :+ movesOfType(movesNo).perform, false, true, dAdvancement, tCriteria)
        } else {
          println(s"Wrong index. $move")
          (derivation, false, false, dAdvancement, tCriteria)
        }
      case s"$move" if moveTypesStrings.contains(move.toLowerCase) && !possibleMoveTypesStrings.contains(move.toLowerCase) =>
        println(s"Move $move not currently applicable.")
        (derivation, false, false, dAdvancement, tCriteria)
      case s"$move $_" if moveTypesStrings.contains(move.toLowerCase) && !possibleMoveTypesStrings.contains(move.toLowerCase) =>
        println(s"Move $move not currently applicable.")
        (derivation, false, false, dAdvancement, tCriteria)

      case _ =>
        println("Error")
        (derivation, false, false, dAdvancement, tCriteria)
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


    val culpritCandidates = framework.culpritsCandidates
    val defences = framework.defences

    println("======================\n" +
            "        GENERAL\n" +
            "======================\n")
    Seq(
      ("Culprit candidates", culpritCandidates),
      ("Culprit candidates contraries", framework.contrariesOf(culpritCandidates)),
      ("Remaining non-blocked rules for proponent", framework.remainingNonBlockedPRules),
      ("Unexpanded prop statements", framework.unexpandedPStatements),
      ("Defences", defences),
      ("Defence contraries", framework.contrariesOf(defences)),
      ("Complete prop arguments", framework.completePiecesP),
      ("J (for complete sem.)", framework.j),
      ("Unexpanded statements for the proponent", framework.unexpandedPStatements),
      ("Played fully expanded statements", framework.fullyExpandedStatements),
      ("Played blocked pieces", framework.playedBlockedPieces),
      ("Unblocked complete played pieces of the opponent", framework.unblockedCompletePlayedPiecesB),
    ).foreach{ case (desc, set) => println(s"$desc:\n\t${set.mkString("; ")}") }

    println("======================\n" +
            " TERMINATION CRITERIA\n" +
            "======================\n")

    TerminationCriteria.seemsToBeWinningDebug
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
