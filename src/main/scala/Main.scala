import aba.framework.Framework
import aba.move.Move
import aba.move.Move.{MoveType, OB1}
import aba.reasoner.{Argument, DisputeState, LiteralArgument, PotentialMove, RuleArgument}

import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    // 1. Create framework from file
    // framework = getFramework

    // 2. Create a list / set of "move" objects, inject framework to them
    // a) use the "factory pattern" i.e. create a Move like Move (enum)
    // for that create the object but moves in separate files
    // use dependency injection so that they have access to framework!!!
    // abstract class "move", constructor with framework
    // methods:
    //  isPossible(state)
    //  perform(state) -> returns B',P'

    // 3. initiate B'. P' (with empty sets / or goals, depending on the definition)

    // 4. Call the recursive function


    //implicit val framework: Framework = new Framework("Wspanialy test") // ???
    //implicit val framework: Framework = new Framework() // ???

    //val move = Move(Move.OB1)
    //move.isPossible(5)
    // move.perform(1)
    // print(Move.values)
    //Move.values.foreach(f => println(s"$f is a ${ if (f.isProponentMove) "proponents" else "opponents"  } move"))
    //???

    // val x  = Calculator("2+2")
    val input = "examples/ex9.4.21.aba"
    implicit val frameworkAba: Framework = Framework("aba", input)
    //implicit val initialState: DisputeState = frameworkAba.initialDState

//    val literal1 = frameworkAba.alphabet.values.head
//    val rule1 = frameworkAba.rules.head
//
//    val arg1 = LiteralArgument(literal1)
//    val arg2 = LiteralArgument(literal1)
//
//    val ruleArg1 = RuleArgument(rule1)
//    val ruleArg2 = RuleArgument(rule1)
//
//    val chwilaPrawdy: Set[Argument] = Set(arg1, arg2, ruleArg1, ruleArg2)




    disputeDerivation(frameworkAba.initialDState :: Nil) // way of creating a list


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
      case Some(info) => println(info)
      case _ =>
    }

    val (newDerivation, stop) = progressDerivation(derivation)
    if (stop) return newDerivation  // TODO: why I need return here -- something with inline ifs and returns

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
