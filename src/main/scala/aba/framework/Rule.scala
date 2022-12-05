package aba.framework


/** Rule class.
 *
 * @param id optional [[Rule]] identifier.
 * @param head head of the [[Rule]].
 * @param body body of the [[Rule]] - set of literals.
 */
case class Rule(id: Option[String], head: String, body: Set[String]) {
  /** Another constructor without the optional id.
   *
   * @param head head.
   * @param body body.
   */
  def this(head: String, body: Set[String]) = {
    this(None, head, body)
  }

  /** Converts the rule to string.
   *
   * @return A string representing the rule.
   */
  override def toString: String = s"$head $ArrowSign ${body.mkString(",")}"

  /** Returns all the statements used within the rule, i.e. the union of the statements from the body and the head.
   *
   * @return Statements contained within the body as well as the head.
   */
  def statements: Set[String] = body + head

  // TODO: find out where is this used and fix this.
  def uid: String = if (hashCode() < 0) "an" + Math.abs(hashCode()).toString else "a" + hashCode().toString

}
