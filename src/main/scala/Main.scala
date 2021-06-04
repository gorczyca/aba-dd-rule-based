import aba.framework.Framework
import aba.move.Move
import aba.move.Move.MoveType
import aba.reasoner.{ DisputeState, PotentialMove }
import commandLineParser.CommandLineParser

import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {

    CommandLineParser.parse(args) match {
      case Some(config) =>
        implicit val framework: Framework = Framework(config.inputFormat, config.inputFilePath)
        disputeDerivation(framework.initialDState :: Nil)
      case _ =>
    }

  }

  @tailrec
  def disputeDerivation(derivation: List[DisputeState])(implicit framework: Framework): List[DisputeState] = {

    // caution
    //implicit def stringToMoveType(moveString: String): MoveType = Move.values.find(_.toString.equalsIgnoreCase(moveString))

    implicit val lastState: DisputeState = derivation.last   // get last derivation state

    println(s"${lastState.id}. ${lastState.move match {
      case Some(value) => s"$value: ${lastState.argument match {
        case Some(arg) => s"$arg"
        case _ => ""
      }}"
      case _ => "(init)"
    }}")

    //  println(s"Arguments:\n\t${framework.decorateArguments.map(_._2).mkString(" ; ")}\n")
    println(s"Dispute state:\n\t${framework.disputeStateToString}\n")
    println(s"Assumptions: \n\t${framework.decorateAssumptions.map(_._2).mkString(" ; ")}\n")
    println(s"Rules:\n\t${framework.decorateRules.map(_._2).mkString(" ; ")}\n")

    implicit val possibleMoves: Map[MoveType, Seq[PotentialMove]] = Move.getPossibleMoves

    framework.checkIfOver match {
      case Some(propWon) => if (propWon) println(s"Game over. ${ if (propWon) "proponent" else "opponent" } won.")
      case _ =>
    }

    val (newDerivation, stop) = progressDerivation(derivation)
    if (stop) return newDerivation

    disputeDerivation(newDerivation)
  }

  def progressDerivation(derivation: List[DisputeState])(implicit framework: Framework,
                                                         possibleMoves: Map[MoveType, Seq[PotentialMove]],
                                                         lastState: DisputeState):
  (List[DisputeState], Boolean) = {

    val digitRegex = """\d+""".r
    val moveTypesStrings = Move.values.map(_.toString.toLowerCase)

    Console.in.readLine match {
      case "?" =>   println(s"Possible moves:\n${Move.possibleMovesToString(possibleMoves)}\n"); (derivation, false)
      case "f" => (forward(derivation), false)
      case s"f $x" if digitRegex.matches(x) => (forward(derivation, x.toInt), false)  // TODO: regexes ?
      case s"f $x" => print(s"Number required, $x passed."); (derivation, false)  // TODO:
      case "b" => (derivation.safeBacktrack(), false)
      case s"b $x" if digitRegex.matches(x) => (derivation.safeBacktrack(x.toInt), false)  // TODO: regexes ?
      case s"b $x" => print(s"Number required, $x passed."); (derivation, false)
      case "q" => (derivation, true)
      case s"$move" if moveTypesStrings.contains(move) => (derivation :+ possibleMoves(move).random.perform, false) // implicit conversion
      case s"$move $x" if moveTypesStrings.contains(move.toLowerCase) && digitRegex.matches(x) =>
        val movesNo = x.toInt
        val movesOfType = possibleMoves(move) // implicit conversion
        if (movesOfType.size >= movesNo + 1) {
           (derivation :+ movesOfType(movesNo).perform, false)
        } else {
          println(s"Wrong index. $move")
          (derivation, false)
        }

      case _ => println("Error"); (derivation, false)
    }
  }


  @tailrec
  private def forward(derivation: List[DisputeState],
                      n: Int = 1)
                     (implicit framework: Framework): List[DisputeState] = {
    implicit val lastState: DisputeState = derivation.last
    lazy val possibleMoves = Move.getPossibleMoves  // lazy val to evaluate it only when n > 0
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
