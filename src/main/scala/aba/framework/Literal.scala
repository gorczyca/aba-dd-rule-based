package aba.framework

case class Literal(id: String) {
  override def toString: String = id

  // TODO: remove this class and use strings only
  override def equals(obj: Any): Boolean = obj match {
    case Literal(id_) => id_ == id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode
}
