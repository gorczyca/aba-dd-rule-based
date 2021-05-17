import aba.framework.Framework
import aba.move.Move
import aba.move.Move.MoveType
import aba.reasoner.DisputeState

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
    val initialState = frameworkAba.initialDState

    disputeDerivation(frameworkAba.initialDState :: Nil)
    //val frameworkApx = Framework("apx", "examples/framework1.apx")
    ???

  }

  @tailrec
  def disputeDerivation(derivation: List[DisputeState])(implicit framework: Framework): Unit = {

    // caution
    //implicit def stringToMoveType(moveString: String): MoveType = Move.values.find(_.toString.equalsIgnoreCase(moveString))


    val lastState = derivation.last   // get last derivation state
    val possibleMoves = Move.values.flatMap(x => Move(x).isPossible(lastState)) // get possible moves



    // get possible moves by enumerating through enum moves, creating by factory the move objects, returning enums that are possible

    // get input from user
    // input match {
    //  backtrack, perform new move, show possible moves, etc
    //

    disputeDerivation(derivation)

  }

}
