package aba.framework


case class Rule(id: Option[String], head: String, body: Set[String]) {
  def this(head: String, body: Set[String]) = {
    this(None, head, body)
  }

  // <- ←
  override def toString: String = s"$head ← ${body.mkString(",")}"

  def statements: Set[String] = body + head

  // TODO: only temporary
  def uid: String = if (hashCode() < 0) "an" + Math.abs(hashCode()).toString else "a" + hashCode().toString

}
