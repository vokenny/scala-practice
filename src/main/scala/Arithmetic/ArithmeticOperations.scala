package Arithmetic

import scala.annotation.tailrec
import math.pow

class S99Int(val num: Int) {

  def getPrimeNumbers: List[Int] = {
    @tailrec
    def removeNonPrimes(checkedPrimes: List[Int], remList: List[Int]): List[Int] = {
      if (remList.head <= Math.sqrt(num)) removeNonPrimes(remList.head :: checkedPrimes, remList.filter(e => e % remList.head != 0))
      else checkedPrimes.reverse ::: remList
    }

    val numsToCheck: List[Int] = List.range(3, num + 1, 2)
    removeNonPrimes(Nil, numsToCheck)
  }

  //P31
  def isPrime: Boolean = {
    // The Sieve of Eratosthenes
    // For finding all the small primes (approx. less than 10,000,000,000).
    // For example, to find all the odd primes less than or equal to 100:
    // List the odd numbers from 3 to 100 (why even list the evens?)
    // The first number is 3 so it is the first odd prime--cross out all of its multiples.
    // Now the first number left is 5, the second odd prime--cross out all of its multiples.
    // Repeat with 7 and then since the first number left, 11,
    // is larger than the square root of 100, all of the numbers left are primes.

    num match {
      case 2 => true
      case i if i >= 3 =>
        if (getPrimeNumbers.contains(i)) true else false
      case _ => false
    }
  }

  //P33
  def isCoprimeTo(x: Int): Boolean = {
    import ArithmeticOperations.gcd
    if (gcd(num, x) == 1) true else false
  }

  //P34
  def totient: Int = {
    // Euler's so-called totient function phi(m) is defined as the number of positive integers r
    // (1 <= r <= m) that are coprime to m

    // (1 to num).count(isCoprimeTo)

    List.range(1, num + 1).fold(0) { (r, e) => if (isCoprimeTo(e)) r + 1 else r }
  }

  //P35
  def primeFactors: List[Int] = {
    // Get list of prime factors for num, that are perfectly divisble (modulus == 0)
    // Divide by max prime
    // Add max prime to result
    // Repeat until number reaches zero

    @tailrec
    def primeFactorsTailRec(recNum: Int, remPrimes: List[Int], result: List[Int]): List[Int] = {
      (recNum, remPrimes) match {
        case (1, _) => result
        case (_, Nil) => result
        case (_, _) =>
          if (recNum % remPrimes.last == 0) primeFactorsTailRec(recNum / remPrimes.last, remPrimes, remPrimes.max :: result)
          else primeFactorsTailRec(recNum, remPrimes.init, result)
      }
    }

    val primeFactors: List[Int] = getPrimeNumbers.filter(e => num % e == 0).sorted
    primeFactorsTailRec(num, primeFactors, Nil)
  }

  //P36
  def primeFactorsMultiplicity: Map[Int, Int] = {
    // Count how many times the prime factor appears

    // Map(primeFactors.map{e => (e, primeFactors.count(_ == e))} : _ *)

    primeFactors.map(e => e -> primeFactors.count(_ == e)).toMap
  }

  //P37
  def totientImproved: Int = {
    // phi(m) = (p1-1)*p1^(m1-1) * (p2-1)*p2^(m2-1) * (p3-1)*p3^(m3-1) * ...

    primeFactorsMultiplicity.foldLeft(1) { (r, e) => (e._1 - 1) * pow(e._1, e._2 - 1).toInt * r }
  }
}

object S99Int {
  implicit def int(i: Int): S99Int = new S99Int(i)
}

object ArithmeticOperations {

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
