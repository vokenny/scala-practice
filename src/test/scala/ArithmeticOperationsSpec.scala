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
}
