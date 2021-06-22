import aba.framework.Framework
import aba.move.Move
import aba.move.Move.MoveType
import aba.reasoner.argumentBased.DisputeStateAB
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
        disputeDerivation(framework.initialDState :: Nil, redraw = true)
      case _ =>
    }

  }

  @tailrec
  def disputeDerivation(derivation: List[DisputeState], redraw: Boolean)(implicit framework: Framework): List[DisputeState] = {

    implicit val lastState: DisputeState = derivation.last   // get last derivation state

    if (redraw) {
      println(lastState.sequenceElement)

      //  println(s"Arguments:\n\t${framework.decorateArguments.map(_._2).mkString(" ; ")}\n")
      println(s"Dispute state:\n\t${framework.disputeStateToString()}\n")
      println(s"Assumptions: \n\t${framework.decorateAssumptions.map(_._2).mkString(" ; ")}\n")
      println(s"Rules:\n\t${framework.decorateRules.map(_._2).mkString(" ; ")}\n")
      println(s"Moves sequence:\n\t${derivation.map(_.sequenceElement).mkString("; ")}\n")
    }


    implicit val possibleMoves: Map[MoveType, Seq[PotentialMove]] = Move.getPossibleMoves

    framework.checkIfOver match {
      case Some(propWon) => println(s"Game over. ${ if (propWon) "Proponent" else "Opponent" } won.")
      case _ =>
    }

    val (newDerivation, stop, nextRedraw) = progressDerivation(derivation)

    if (!stop) disputeDerivation(newDerivation, nextRedraw)
    else newDerivation
  }

  def progressDerivation(derivation: List[DisputeState])(implicit framework: Framework,
                                                         possibleMoves: Map[MoveType, Seq[PotentialMove]],
                                                         lastState: DisputeState):
  (List[DisputeState], Boolean, Boolean) = {

    val digitRegex = """\d+""".r
    val moveTypesStrings = Move.values.map(_.toString.toLowerCase)

    Console.in.readLine match {
      case "?" => println(s"Possible moves:\n${Move.possibleMovesToString(possibleMoves)}\n"); (derivation, false, false)
      case s"dot" =>
        val fileName = DotConverter.exportDotRepr()
        println(s"Graph representation exported to: $fileName")
        (derivation, false, false)
      case s"dot $fileName" =>
        println(s"Graph representation exported to: $fileName")
        (derivation, false, false)
      case "a" =>
        val dStateAB = DisputeStateAB(derivation)
        println(s"Dispute state (Argument-Based):\n\t${framework.disputeStateToString(Some(dStateAB.decorateArguments.map(_._2).mkString("; ")))}\n")
        println(s"Rules:\n\t${dStateAB.decorateRules.map(_._2).mkString("; ")}\n")
        (derivation, false, false)
      case "f" => (forward(derivation), false, true)
      case s"f $x" if digitRegex.matches(x) => (forward(derivation, x.toInt), false, true)  // TODO: regexes ?
      case s"f $x" => print(s"Number required, $x passed."); (derivation, false, true)  // TODO:
      case "b" => (derivation.safeBacktrack(), false, true)
      case "bb" => (derivation.take(1), false, true)
      case s"b $x" if digitRegex.matches(x) => (derivation.safeBacktrack(x.toInt), false, true)  // TODO: regexes ?
      case s"b $x" => print(s"Number required, $x passed."); (derivation, false, true)
      case "q" => (derivation, true, false)
      case s"$move" if moveTypesStrings.contains(move) => (derivation :+ possibleMoves(move).random.perform, false, true) // implicit conversion
      case s"$move $x" if moveTypesStrings.contains(move.toLowerCase) && digitRegex.matches(x) =>
        val movesNo = x.toInt
        val movesOfType = possibleMoves(move) // implicit conversion
        if (movesOfType.size >= movesNo + 1) {
           (derivation :+ movesOfType(movesNo).perform, false, true)
        } else {
          println(s"Wrong index. $move")
          (derivation, false, false)
        }

      case _ => println("Error"); (derivation, false, false)
    }
  }


  @tailrec
  private def forward(derivation: List[DisputeState],
                      n: Int = 1)
                     (implicit framework: Framework): List[DisputeState] = {
    implicit val lastState: DisputeState = derivation.last
    implicit lazy val possibleMoves: Map[MoveType, Seq[PotentialMove]] = Move.getPossibleMoves  // lazy val to evaluate it only when n > 0

    framework.checkIfOver match {
      case Some(_) => return derivation
      case _ =>
    }

    println(s"$n moves left.")

    if (n == 0 || possibleMoves.isEmpty) derivation  // TODO: add checks for won game
    else {
      val newDState = possibleMoves.randomElement.perform
      forward(derivation :+ newDState, n-1) // tail recursion
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
