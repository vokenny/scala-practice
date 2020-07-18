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

}
