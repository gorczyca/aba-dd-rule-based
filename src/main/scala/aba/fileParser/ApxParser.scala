package aba.fileParser

import aba.framework.{ Contrary, Rule }

/** Parser for *.apx files.
 *
 * Parses the framework in the following form:
 *
 * rule(HEAD, [BODY]).
 *  with HEAD being a single statement and BODY a set thereof.
 *
 * contrary(ASSUMPTION, CONTRARY).
 *  with ASSUMPTION, CONTRARY being statements.
 *
 * assumption(ASSUMPTION).
 *  with ASSUMPTION being a statement.
 *
 * goal(GOAL).
 *  with GOAL being a statement.
 *
 * constraint(CONSTRAINT)
 *  with CONSTRAINT being a statement.
 */
object ApxParser extends FileParser {

  /** Parses a rule.
   *
   * rule(HEAD, [BODY]).
   * with HEAD being a single statement and BODY a set thereof.
   *
   * E.g.
   *  rule(h1,[a1,b2,c3]).
   *
   *  @return Rule parser.
   */
  override def rule: Parser[Rule] = "rule("~identifier~",["~fields~"])." ^^ { case "rule("~head~",["~body~"])." => new Rule(head, body)  }

  /** Parses a contrary.
   *
   * contrary(ASSUMPTION, CONTRARY).
   * with ASSUMPTION, CONTRARY being statements.
   *
   * E.g.
   *  contrary(a1,x2).
   *
   *  @return Contrary parser.
   */
  override def contrary: Parser[Contrary] = "contrary("~identifier~","~identifier~")." ^^ { case "contrary("~ass~","~ctr~")." => Contrary(ass, ctr) }

  /** Parses an assumption.
   *
   * assumption(ASSUMPTION).
   * with ASSUMPTION being a statement.
   *
   * E.g.
   *  assumption(a1).
   *
   *  @return Assumption statement parser.
   */
  override def assumption: Parser[String] = "asm("~identifier~")." ^^ { case "asm("~ass~")." => ass }

  /** Parses a goal.
   *
   * goal(GOAL).
   * with GOAL being a statement.
   *
   * E.g.
   *  goal(h1).
   *
   *  @return Goal statement parser.
   */
  override def goal: Parser[String] = "goal("~identifier~")." ^^ { case "goal("~goal~")." => goal }

  /** Parses a constraint.
   *
   * constraint(CONSTRAINT)
   * with CONSTRAINT being a statement.
   *
   * E.g.
   *  constraint(a2).
   *
   *  @return Constraint statement parser.
   */
  override def constraint: Parser[String] = "constraint("~identifier~")." ^^ { case "constraint("~con~")." => con }


}
