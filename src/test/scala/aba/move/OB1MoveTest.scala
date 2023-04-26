package aba.move

import aba.framework.Rule
import aba.reasoner.DisputeState
import aba.move.DisputeAdvancement.{DAB, DABF, DC, DF, DS}
import org.scalatest.funsuite.AnyFunSuite


class OB1MoveTest extends AnyFunSuite {

  //testInitialState()

  testDABContinuedState()



  def testInitialState(): Unit = {
    val bicycleFramework = FrameworkMock.bicycleFramework
    val initialState = DisputeState.initial(bicycleFramework)

    // No moves possible
    test("OB1Move.isPossible - DC, initial") {
      val possibleOB1Moves = OB1Move.isPossible(DC)(bicycleFramework, initialState)
      assert(possibleOB1Moves.isEmpty)
    }

    test("OB1Move.isPossible - DAB, initial") {
      val possibleOB1Moves = OB1Move.isPossible(DAB)(bicycleFramework, initialState)
      assert(possibleOB1Moves.isEmpty)
    }

    test("OB1Move.isPossible - DABF, initial") {
      val possibleOB1Moves = OB1Move.isPossible(DABF)(bicycleFramework, initialState)
      assert(possibleOB1Moves.isEmpty)
    }

    test("OB1Move.isPossible - DS, initial") {
      val possibleOB1Moves = OB1Move.isPossible(DS)(bicycleFramework, initialState)
      assert(possibleOB1Moves.isEmpty)
    }

    test("OB1Move.isPossible - DF, initial") {
      val possibleOB1Moves = OB1Move.isPossible(DF)(bicycleFramework, initialState)
      assert(possibleOB1Moves.isEmpty)
    }
  }

  def testDABContinuedState(): Unit = {
    val bicycleFramework = FrameworkMock.bicycleFramework
    val initialState = DisputeState.initial(bicycleFramework)

    // move sequence
    val moveSequence = Seq(
      PB1Move(Rule(None, "xWheelsWide", Set("wheelsSkinny")), None),
      PB1Move(Rule(None, "wheelsSkinny", Set("w4WheelsSkinny")), None),
      OB2Move(Rule(None, "xWheelsSkinny", Set("wheelsWide")), None)
    )

    val newState = MoveSequencePerformer(moveSequence, initialState, bicycleFramework)

    val possibleOB1Moves = OB1Move.isPossible(DAB)(bicycleFramework, newState)

    test("OB1Move.isPossible - DAB, ctd") {

      val expectedRule = Rule(None, "wheelsWide", Set("typeMountain"))
      assert(possibleOB1Moves match {
        case OB1Move(`expectedRule`, _) :: Nil => true
        case _ => false
      })
    }
  }

  def testDABFContinuedState(): Unit = {
    // TODO:
  }

}
