package aba.move

import aba.framework.Framework

object OB1Move extends Move {

  override def isPossible(dummy: Int)(implicit framework: Framework): Boolean = {
    framework.printSth()
    true
  }

}
