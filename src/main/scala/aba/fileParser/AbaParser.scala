package aba.fileParser

import aba.framework.{Contrary, Rule}

/** Parser for *.aba files.
 *
 * Parses the framework in the following form:
 *
 * % Rule
 * [ID:]HEAD:-BODY.
 *  with ID being an optional literal, HEAD a literal and BODY a set thereof.
 *
 * % Contrary
 * ASSUMPTION:CONTRARY.
 *  with ASSUMPTION and CONTRARY being literals.
 *
 * % Assumption
 * ASSUMPTION.
 *  with assumption being a literal.
 *
 * % Goal
 * ?GOAL.
 *  with GOAL being a literal.
 *
 * % Constraint
 * !CONSTRAINT.
 *  with CONSTRAINT being a literal.
 *
 */
object AbaParser extends FileParser {

    /** Parses a rule.
     *
     * [ID:]HEAD:-BODY.
     * with ID being an optional literal, HEAD a literal and BODY a set thereof.
     *
     * E.g.
     *  rule1:h1:-a1,b2,c3.
     *  h1:-a1,b2,c3.
     *
     *  @return Rule parser.
     */
    override def rule: Parser[Rule] = opt(identifier <~ ": ") ~ identifier~":-"~fields~"." ^^ {
        case Some(rule_id)~head~":-"~body~"." => Rule(Some(rule_id), head, body)
        case None ~ head~":-"~body~"." => Rule(None, head, body)
    }

    /** Parses a contrary.
     *
     * ASSUMPTION:CONTRARY.
     * with ASSUMPTION and CONTRARY being literals.
     *
     * E.g.
     *  a1:x2.
     *
     *  @return Contrary parser.
     */
    override def contrary: Parser[Contrary] = identifier~":"~identifier~"." ^^ { case ass~":"~ctr~"." => Contrary(ass, ctr) }

    /** Parses an assumption.
     *
     * ASSUMPTION.
     * with ASSUMPTION being a literal.
     *
     * E.g.
     *  a1.
     *
     *  @return Assumption string parser.
     */
    override def assumption: Parser[String] = identifier~"." ^^ { case ass~"." => ass }

    /** Parses a goal.
     *
     * ?GOAL.
     * with GOAL being a literal.
     *
     * E.g.
     *  ?h1.
     *
     *  @return Goal string parser.
     */
    override def goal: Parser[String] = "?"~identifier~"." ^^ { case "?"~goal~"." => goal }

    /** Parses a constraint.
     *
     * !CONSTRAINT.
     * with CONSTRAINT being a literal.
     *
     * !a2.
     *
     *  @return Constraint string parser.
     */
    override def constraint: Parser[String] = "!"~identifier~"." ^^ { case "!"~con~"." => con }

}

