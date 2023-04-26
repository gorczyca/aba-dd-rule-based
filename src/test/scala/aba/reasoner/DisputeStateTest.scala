package aba.reasoner

import aba.framework.Rule
import org.scalatest.funsuite.AnyFunSuite


class DisputeStateTest extends AnyFunSuite {


  val pStatements = Set("a", "b", "c")
  val pRules = Set(
    new Rule("a", Set("r", "d")),
    new Rule("d", Set("g"))
  )

  test("DisputeState.calculatePPlayedUnexpandedStatements") {
    assert(DisputeState.calculatePPlayedUnexpandedStatements(pStatements, pRules) === Set("b", "c"))
  }
}
