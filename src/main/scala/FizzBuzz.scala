object FizzBuzz extends App {

//  For a given range of numbers, print them all out
//  where all numbers divisible by three are replaced with "Fizz"
//  and all numbers divisible by five are replaced with "Buzz"
//  and all numbers divisible by both are replaced with "FizzBuzz"

  def fizzBuzzMate(numbers: Range): List[Any] = {
    numbers.map {
      case i if (i % 3 == 0) && (i % 5 == 0) => "FizzBuzz"
      case i if i % 3 == 0 => "Fizz"
      case i if i % 5 == 0 => "Buzz"
      case i => i
    }.toList
  }
}
