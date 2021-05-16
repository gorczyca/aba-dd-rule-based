package aba.move

class OB1Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("OB1 move")
    100
  }
}
