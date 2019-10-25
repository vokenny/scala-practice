import scala.annotation.tailrec

object ArithmeticOperations {

  //P31
  def isPrime(num: Int): Boolean = {
    // The Sieve of Eratosthenes
    // For finding all the small primes (approx. less than 10,000,000,000).
    // For example, to find all the odd primes less than or equal to 100:
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

  //P32
  def gcd(x1: Int, x2: Int): Int = {
    // For example, 21 is the GCD of 252 and 105 (as 252 = 21 × 12 and 105 = 21 × 5),
    // and the same number 21 is also the GCD of 105 and 252 − 105 = 147.
    // Since this replacement reduces the larger of the two numbers,
    // repeating this process gives successively smaller pairs of numbers until the
    // two numbers become equal. When that occurs, they are the GCD of the original two numbers.

    @tailrec
    def gcdTailRec(numToReduce: Int, remainder: Int): Int = {
      if (numToReduce % remainder == 0) remainder
      else gcdTailRec(remainder, numToReduce % remainder)
    }

    (Math.abs(x1), Math.abs(x2)) match {
      case (_, 0) => x1
      case (0, _) => x2
      case (a, b) => gcdTailRec(a, b)
    }
  }
}
