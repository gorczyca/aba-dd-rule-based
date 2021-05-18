import aba.framework.Framework
import aba.move.Move
import aba.move.Move.MoveType
import aba.reasoner.{Argument, DisputeState, LiteralArgument, RuleArgument}

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
    val initialState = frameworkAba.initialDState

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




    disputeDerivation(frameworkAba.initialDState :: Nil)  // way of creating a list


  }

  @tailrec
  def disputeDerivation(derivation: List[DisputeState])(implicit framework: Framework): Unit = {

    // caution
    //implicit def stringToMoveType(moveString: String): MoveType = Move.values.find(_.toString.equalsIgnoreCase(moveString))


    implicit val lastState: DisputeState = derivation.last   // get last derivation state
    val possibleMoves = Move.values.flatMap(x => Move(x).isPossible) // get possible moves

    val newState = possibleMoves.head.perform



    // get possible moves by enumerating through enum moves, creating by factory the move objects, returning enums that are possible

    // get input from user
    // input match {
    //  backtrack, perform new move, show possible moves, etc
    //

    disputeDerivation(derivation :+ newState)

  }

}
