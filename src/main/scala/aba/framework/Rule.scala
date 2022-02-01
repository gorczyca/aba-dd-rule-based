package aba.framework


case class Rule(id: Option[String], head: Literal, body: Set[Literal]) {
  def this(head: Literal, body: Set[Literal]) = {
    this(None, head, body)
  }

  // <- ←
  //override def toString: String = s"$head :- ${body.mkString(",")}"
  override def toString: String = s"$head ← ${body.mkString(",")}"

  def statements: Set[Literal] = body + head
}
