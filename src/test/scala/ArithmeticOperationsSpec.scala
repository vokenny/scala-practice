import Arithmetic.ArithmeticOperations
import Arithmetic.ArithmeticOperations._
import org.scalatest.matchers.should.Matchers
import Arithmetic.S99Int._
import org.scalatest.flatspec.AnyFlatSpec


class ArithmeticOperationsSpec extends AnyFlatSpec with Matchers {

  "P31 Arithmetic Operations isPrime" should "return true for a Prime number" in {
    2.isPrime should be (true)
    3.isPrime should be (true)
    5.isPrime should be (true)
    13.isPrime should be (true)
  }

  it should "return false for a non-Prime number" in {
    (-3).isPrime should be (false)
    (-1).isPrime should be (false)
    0.isPrime should be (false)
    1.isPrime should be (false)
    9.isPrime should be (false)
  }

  "P32 Arithmetic Operations gcd" should "return greatest common denominator" in {
    gcd(-10, 250) should be (10)
    gcd(36, 63) should be (9)
    gcd(105, 252) should be (21)
    gcd(252, 105) should be (21)
    gcd(45678, 43890) should be (6)
  }

  it should "return the other value if it is 0" in {
    gcd(0, 1234) should be (1234)
    gcd(67890, 0) should be (67890)
    gcd(0, 0) should be (0)
  }

  "P33 Arithmetic Operations isCoprime" should "return true" in {
    35.isCoprimeTo(64) should be (true)
  }

  it should "return false" in {
    16.isCoprimeTo(32) should be (false)
  }

  "P34 Arithmetic Operations totient" should "return count of +ve integers that are coprime" in {
    10.totient should be (4)
    15.totient should be (8)
  }

  "P35 Arithmetic Operations primeFactors" should "return list of prime factors" in {
    315.primeFactors should be (List(3, 3, 5, 7))
    460.primeFactors should be (List(5, 23))
  }

  "P36 Arithmetic Operations primeFactorMultiplicity" should "return map of prime factors with their multiplicity" in {
    315.primeFactorsMultiplicity should be (Map(3 -> 2, 5 -> 1, 7 -> 1))
    460.primeFactorsMultiplicity should be (Map(5 -> 1, 23 -> 1))
  }

  "P37 Arithmetic Operations totientImproved" should "return count of +ve integers that are coprime" in {
    10.totientImproved should be (4)
    15.totientImproved should be (8)
  }

  "P39 Arithmetic Operations listPrimesInRange" should "return list of prime numbers in the given range" in {
    ArithmeticOperations.listPrimesInRange(7 to 31) should be (List(7, 11, 13, 17, 19, 23, 29, 31))
    ArithmeticOperations.listPrimesInRange(1 to 15) should be (List(2, 3, 5, 7, 11, 13))
    ArithmeticOperations.listPrimesInRange(-20 to 15) should be (List(2, 3, 5, 7, 11, 13))
  }

  it should "return empty list for range below 2" in {
    ArithmeticOperations.listPrimesInRange(-20 to 1) should be (List())
  }
}
