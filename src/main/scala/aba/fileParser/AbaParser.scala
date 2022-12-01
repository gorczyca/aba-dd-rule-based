package aba.fileParser

import aba.framework.{Contrary, Rule}

object AbaParser extends FileParser {

    override def rule: Parser[Rule] = opt(identifier <~ ": ") ~ identifier~":-"~fields~"." ^^ {
        case Some(rule_id)~head~":-"~body~"." => Rule(Some(rule_id), head, body)
        case None ~ head~":-"~body~"." => Rule(None, head, body)
    }
    override def contrary: Parser[Contrary] = identifier~":"~identifier~"." ^^ { case ass~":"~ctr~"." => Contrary(ass, ctr) }
    override def assumption: Parser[String] = identifier~"." ^^ { case ass~"." => ass }
    override def goal: Parser[String] = "?"~identifier~"." ^^ { case "?"~goal~"." => goal }
    override def constraint: Parser[String] = "!"~identifier~"." ^^ { case "!"~con~"." => con }

}

