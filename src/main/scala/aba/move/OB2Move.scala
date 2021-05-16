package aba.move

class OB2Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("OB2 move")
    100
  }}
