import Arithmetic.ArithmeticOperations
import org.scalatest.{FlatSpec, Matchers}

class ArithmeticOperationsSpec extends FlatSpec with Matchers {

  "P31 Arithmetic Operations isPrime" should "return true for a Prime number" in {
    ArithmeticOperations.isPrime(2) should be (true)
    ArithmeticOperations.isPrime(3) should be (true)
    ArithmeticOperations.isPrime(5) should be (true)
    ArithmeticOperations.isPrime(13) should be (true)
  }

  it should "return false for a non-Prime number" in {
    ArithmeticOperations.isPrime(-3) should be (false)
    ArithmeticOperations.isPrime(-1) should be (false)
    ArithmeticOperations.isPrime(0) should be (false)
    ArithmeticOperations.isPrime(1) should be (false)
    ArithmeticOperations.isPrime(9) should be (false)
  }

  "P32 Arithmetic Operations gcd" should "return greatest common denominator" in {
    ArithmeticOperations.gcd(-10, 250) should be (10)
    ArithmeticOperations.gcd(36, 63) should be (9)
    ArithmeticOperations.gcd(105, 252) should be (21)
    ArithmeticOperations.gcd(252, 105) should be (21)
    ArithmeticOperations.gcd(45678, 43890) should be (6)
  }

  it should "return the other value if it is 0" in {
    ArithmeticOperations.gcd(0, 1234) should be (1234)
    ArithmeticOperations.gcd(67890, 0) should be (67890)
    ArithmeticOperations.gcd(0, 0) should be (0)
  }

  "P33 Arithmetic Operations isCoprime" should "return true" in {
    ArithmeticOperations.isCoprime(35, 64) should be (true)
  }

  it should "return false" in {
    ArithmeticOperations.isCoprime(16, 32) should be (false)
  }
}
