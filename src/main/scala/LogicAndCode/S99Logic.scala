package LogicAndCode
import java.io

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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

  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    for {
      a <- List(true, false)
      b <- List(true, false)
    } printf("%-5s %-5s %-5s\n", a, b, f(a, b))
  }

  //P49
  // This solution does not output Gray Code in the right order
  def grayCodeUnordered(length: Int): List[String] = {
    def generateVariations(code: Option[String]): List[String] = {
      code match {
        case Some(v) =>
          if (v.length == length) List(v)
          else {
            val varsAdding0: List[String] = generateVariations(Some(v + "0"))
            val varsAdding1: List[String] = generateVariations(Some(v + "1"))
            (varsAdding0 ::: varsAdding1).distinct
          }
        case None =>
          val varsStarting0: List[String] = generateVariations(Some("0"))
          val varsStarting1: List[String] = generateVariations(Some("1"))
          (varsStarting0 ::: varsStarting1).distinct
      }
    }

    generateVariations(None)
  }

  def grayCode(length: Int): List[String] = {
    def generate(originalList: List[String]): List[String] = {
      if (originalList.forall(_.length == length)) originalList
      else {
        val prefixed = originalList.map(c => "0" + c)
        val prefixedRev = originalList.reverse.map(c => "1" + c)
        generate(prefixed ::: prefixedRev)
      }
    }

    if (length > 0) generate(List("0", "1")) else List()
  }

}
