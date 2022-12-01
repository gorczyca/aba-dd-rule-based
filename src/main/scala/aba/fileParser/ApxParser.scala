package aba.fileParser

import aba.framework.{ Contrary, Rule }


object ApxParser extends FileParser {

  override def rule: Parser[Rule] = "rule("~identifier~",["~fields~"])." ^^ { case "rule("~head~",["~body~"])." => new Rule(head, body)  }
  override def contrary: Parser[Contrary] = "contrary("~identifier~","~identifier~")." ^^ { case "contrary("~ass~","~ctr~")." => Contrary(ass, ctr) }
  override def assumption: Parser[String] = "asm("~identifier~")." ^^ { case "asm("~ass~")." => ass }
  override def goal: Parser[String] = "goal("~identifier~")." ^^ { case "goal("~goal~")." => goal }
  override def constraint: Parser[String] = "constraint("~identifier~")." ^^ { case "constraint("~con~")." => con }


}
