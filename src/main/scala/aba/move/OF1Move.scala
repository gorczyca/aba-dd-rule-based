package aba.move

class OF1Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("OF1 move")
    100
  }}
