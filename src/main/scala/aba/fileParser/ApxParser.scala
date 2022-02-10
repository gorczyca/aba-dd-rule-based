package aba.fileParser

import aba.framework.{ Contrary, Rule }


object ApxParser extends FileParser {

  // TODO: wtf, why when I use new I can use not default constructor
  override def rule: Parser[Rule] = "rule("~literal~",["~fields~"])." ^^ { case "rule("~head~",["~body~"])." => new Rule(head.id, body.map(_.id))  }
  override def contrary: Parser[Contrary] = "contrary("~literal~","~literal~")." ^^ { case "contrary("~ass~","~ctr~")." => Contrary(ass.id, ctr.id) }
  override def assumption: Parser[PAssumption] = "asm("~literal~")." ^^ { case "asm("~ass~")." => PAssumption(ass) }
  override def goal: Parser[PGoal] = "goal("~literal~")." ^^ { case "goal("~goal~")." => PGoal(goal) }
  override def constraint: Parser[PConstraint] = "constraint("~literal~")." ^^ { case "constraint("~con~")." => PConstraint(con) }


}
