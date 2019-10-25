import scala.annotation.tailrec

object ArithmeticOperations {

  //P31
  def isPrime(num: Int): Boolean = {
    // The Sieve of Eratosthenes
    // For finding all the small primes, approx. less than 10,000,000,000
    // For example, to find all the odd primes less than or equal to 100
    // List the odd numbers from 3 to 100 (why even list the evens?)
    // The first number is 3 so it is the first odd prime--cross out all of its multiples.
    // Now the first number left is 5, the second odd prime--cross out all of its multiples.
    // Repeat with 7 and then since the first number left, 11,
    // is larger than the square root of 100, all of the numbers left are primes.

    @tailrec
    def removeNonPrimes(checkedPrimes: List[Int], remList: List[Int]): List[Int] = {
      if (remList.head <= Math.sqrt(num)) removeNonPrimes(remList.head :: checkedPrimes, remList.filter(e => e % remList.head != 0))
      else checkedPrimes.reverse ::: remList
    }

    num match {
      case 2 => true
      case i if i >= 3 =>
        val numsToCheck: List[Int] = List.range(3, i + 1, 2)
        if (removeNonPrimes(Nil, numsToCheck).contains(i)) true else false
      case _ => false
    }
  }
}
