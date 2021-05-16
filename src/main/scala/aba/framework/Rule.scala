package aba.framework


case class Rule(id: Option[String], head: Literal, body: Set[Literal]) {
  def this(head: Literal, body: Set[Literal]) = {
    this(None, head, body)
  }
}
