import LogicAndCode.S99Logic
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class S99LogicSpec extends AnyFlatSpec with Matchers {

  Seq((true, true, true),
    (true, false, false),
    (false, true, false),
    (false, false, false)).foreach { case (x, y, r) =>
    "P46 S99Logic and" should s"return $r for inputs ($x, $y)" in {
      S99Logic.and(x, y) should be (r)
    }
  }

  Seq((true, true, true),
    (true, false, true),
    (false, true, true),
    (false, false, false)).foreach { case (x, y, r) =>
    "P46 S99Logic or" should s"return $r for inputs ($x, $y)" in {
      S99Logic.or(x, y) should be (r)
    }
  }

  Seq((true, true, false),
    (true, false, false),
    (false, true, false),
    (false, false, true)).foreach { case (x, y, r) =>
    "P46 S99Logic nand" should s"return $r for inputs ($x, $y)" in {
      S99Logic.nand(x, y) should be (r)
    }
  }

  Seq((true, true, false),
    (true, false, true),
    (false, true, true),
    (false, false, true)).foreach { case (x, y, r) =>
    "P46 S99Logic nor" should s"return $r for inputs ($x, $y)" in {
      S99Logic.nor(x, y) should be (r)
    }
  }

  Seq((true, true, false),
    (true, false, true),
    (false, true, true),
    (false, false, false)).foreach { case (x, y, r) =>
    "P46 S99Logic xor" should s"return $r for inputs ($x, $y)" in {
      S99Logic.xor(x, y) should be (r)
    }
  }

  Seq((true, true, true),
    (true, false, false),
    (false, true, true),
    (false, false, true)).foreach { case (x, y, r) =>
    "P46 S99Logic impl" should s"return $r for inputs ($x, $y)" in {
      S99Logic.impl(x, y) should be (r)
    }
  }

  Seq((true, true, true),
    (true, false, false),
    (false, true, false),
    (false, false, true)).foreach { case (x, y, r) =>
    "P46 S99Logic equ" should s"return $r for inputs ($x, $y)" in {
      S99Logic.equ(x, y) should be (r)
    }
  }

  "P49 S99Logic grayCodeUnordered" should "return List(\"0\", \"1\")" in {
    S99Logic.grayCodeUnordered(1) should be (List("0", "1"))
  }

  "P49 S99Logic grayCodeUnordered" should "return all two-bit Gray Codes" in {
    val grayCodes = List("00", "01", "11", "10")
    grayCodes.foreach(c => S99Logic.grayCodeUnordered(2) should contain (c))
  }

  "P49 S99Logic grayCode" should "return List(\"0\", \"1\")" in {
    S99Logic.grayCode(1) should be (List("0", "1"))
  }

  "P49 S99Logic grayCode" should "return all two-bit Gray Codes" in {
    val grayCodes = List("00", "01", "11", "10")
    S99Logic.grayCode(2) should be (grayCodes)
  }

  "P49 S99Logic grayCode" should "return all three-bit Gray Codes" in {
    val grayCodes = List("000", "001", "011", "010", "110", "111", "101", "100")
    S99Logic.grayCode(3) should be (grayCodes)
  }

}
