package aba.move

class OF2Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("OF2 move")
    100
  }}
