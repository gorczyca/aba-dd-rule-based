package aba.fileParser

import aba.framework.{Contrary, Rule}

object AbaParser extends FileParser {

    override def rule: Parser[Rule] = opt(identifier <~ ": ") ~ literal~":-"~fields~"." ^^ {
        case Some(rule_id)~head~":-"~body~"." => Rule(Some(rule_id), head, body)
        case None ~ head~":-"~body~"." => Rule(None, head, body)
    }
    override def contrary: Parser[Contrary] = literal~":"~literal~"." ^^ { case ass~":"~ctr~"." => Contrary(ass, ctr) }
    override def assumption: Parser[PAssumption] = literal~"." ^^ { case ass~"." => PAssumption(ass) }
    override def goal: Parser[PGoal] = "?"~literal~"." ^^ { case "?"~goal~"." => PGoal(goal) }
    override def constraint: Parser[PConstraint] = "!"~literal~"." ^^ { case "!"~con~"." => PConstraint(con) }

}

