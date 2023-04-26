package aba.fileParser
import aba.framework.{Contrary, Rule}

object IccmaParser extends FileParser {

  /** Returns a parser of a rule.
   * TODO: this can be done better
   * r HEAD BODY
   * with HEAD being a statement and BODY a space separated set of statements
   *
   * E.g.
   * r 5 1 2 3
   * standing for a rule
   * 5 <- 1,2,3
   *
   * @return Rule parser.
   */
  override def rule: Parser[Rule] = "r" ~> identifier ~ """.*""".r ^^ { case h ~ b => new Rule(h, b.trim.split("\\s+").toSet)   }




  /** Returns a parser of a contrary.
   *
   * c ASSUMPTION CONTRARY
   * with ASSUMPTION, CONTRARY being statements.
   *
   * E.g.
   * c 1 2
   *
   * @return Contrary parser.
   */
  override def contrary: Parser[Contrary] = "c" ~> identifier ~ identifier ^^ { case a ~ c => Contrary(a, c) }

  /** Returns a string parser of a contrary.
   *
   * a ASSUMPTION
   * with ASSUMPTION being a statement
   *
   * E.g.
   * a 1
   *
   * @return Assumption string parser.
   */
  override def assumption: Parser[String] = "a" ~> identifier ^^ { a => a }

  /** Returns a string parser of a goal.
   *
   * g GOAL
   * with GOAL being a statement
   *
   * E.g.
   * g 1
   *
   * @return Goal string parser.
   */
  override def goal: Parser[String] = "g" ~> identifier ^^ { g => g }

  /** Returns a string parser of a constraint.
   *
   * x CONSTRAINT
   * with CONSTRAINT being a statement
   *
   * E.g.
   * x 1
   *
   * @return Constraint string parser.
   */
  override def constraint: Parser[String] = "x" ~> identifier ^^ { x => x }
}
