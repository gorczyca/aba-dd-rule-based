package aba.fileParser

import aba.framework.{Contrary, Rule}

object AbaParser extends FileParser {

    // TODO:
    override def rule: Parser[Rule] = opt(identifier <~ ": ") ~ literal~":-"~fields~"." ^^ {
        case Some(rule_id)~head~":-"~body~"." => Rule(Some(rule_id), head.id, body.map(_.id))
        case None ~ head~":-"~body~"." => Rule(None, head.id, body.map(_.id))
    }
    override def contrary: Parser[Contrary] = literal~":"~literal~"." ^^ { case ass~":"~ctr~"." => Contrary(ass.id, ctr.id) }
    override def assumption: Parser[PAssumption] = literal~"." ^^ { case ass~"." => PAssumption(ass) }
    override def goal: Parser[PGoal] = "?"~literal~"." ^^ { case "?"~goal~"." => PGoal(goal) }
    override def constraint: Parser[PConstraint] = "!"~literal~"." ^^ { case "!"~con~"." => PConstraint(con) }

}

