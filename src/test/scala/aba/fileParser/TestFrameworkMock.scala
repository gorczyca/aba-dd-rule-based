package aba.fileParser

import aba.framework.{Contrary, Framework, Rule}

object TestFrameworkMock {

  val frameworkMock: Framework = Framework(
    rules = Set(
      new Rule("p", Set("q")),
      new Rule("q", Set("a")),
      new Rule("r", Set("p")),
      new Rule("t", Set("b")),
      new Rule("t", Set("p","s")),
      new Rule("t", Set("q","u","d")),
    ),
    assumptions = Set("a", "b", "c", "d"),
    goals = Set("p"),
    contraries = Set(
      Contrary("a", "t"),
      Contrary("b", "r"),
      Contrary("c", "t"),
      Contrary("d", "c"),
    ),
    constraints = Set()
  )

}
