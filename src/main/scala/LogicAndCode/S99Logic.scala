package LogicAndCode

object S99Logic {

  //P46
  def and(x: Boolean, y: Boolean): Boolean = if (x) y else x

  def or(x: Boolean, y: Boolean): Boolean = if (x) true else y

  def nand(x: Boolean, y: Boolean): Boolean = {
    if (x) false else {
      if (y) false else true
    }
  }

  def nor(x: Boolean, y: Boolean): Boolean = {
    if (x) {
      if (y) false else true
    } else true
  }

  def xor(x: Boolean, y: Boolean): Boolean = {
    if (x) {
      if (y) false else true
    } else y
  }

  def impl(x: Boolean, y: Boolean): Boolean = {
    if (x) y else true
  }

  def equ(x: Boolean, y: Boolean): Boolean = {
    if (x) y else {
      if (y) x else true
    }
  }
}
