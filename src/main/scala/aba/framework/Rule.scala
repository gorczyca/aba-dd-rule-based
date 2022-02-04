package aba.framework


case class Rule(id: Option[String], head: String, body: Set[String]) {
  def this(head: String, body: Set[String]) = {
    this(None, head, body)
  }

  // <- ←
  override def toString: String = s"$head ← ${body.mkString(",")}"

  def statements: Set[String] = body + head
}
