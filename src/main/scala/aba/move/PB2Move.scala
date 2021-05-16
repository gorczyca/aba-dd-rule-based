package aba.move

class PB2Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("PB2 move")
    100
  }}
