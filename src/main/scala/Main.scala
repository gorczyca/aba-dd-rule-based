import aba.framework.Framework
import aba.move.Move

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


    implicit val framework: Framework = new Framework("Wspanialy test") // ???
    //implicit val framework: Framework = new Framework() // ???

    val move = Move(Move.OB1)
    move.isPossible(5)
    // move.perform(1)
    // print(Move.values)
    Move.values.foreach(f => println(s"$f is a ${ if (f.isProponentMove) "proponents" else "opponents"  } move"))
    //???


  }

//  def derivation(derivation: (Int, Float), framework): (Int, Float) = {
//
//    // get possible moves by enumerating through enum moves, creating by factory the move objects, returning enums that are possible
//
//    // get input from user
//    // input match {
//    //  backtrack, perform new move, show possible moves, etc
//    //
//
//
//  }

}
