import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FizzBuzzSpec extends AnyFlatSpec with Matchers {

  "fizzBuzzMate" should "return correct list of numbers with Fizzes and Buzzes" in {
    FizzBuzz.fizzBuzz(1 to 15) should be (List(1, 2, "Fizz", 4, "Buzz", "Fizz", 7, 8, "Fizz", "Buzz", 11, "Fizz", 13, 14, "FizzBuzz"))
  }
}
