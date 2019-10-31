package Arithmetic

import S99Int._
import ArithmeticOperations._

object ArithmeticProblems extends App {

  println("\nP31 (**) Determine whether a given integer number is prime")
  println(89.isPrime)
  println(100.isPrime)

  println("\nP32 (**) Determine the greatest common divisor of two positive integer numbers")
  println(gcd(-2354, 250))

  println("\nP33 (*) Determine whether two positive integer numbers are coprime")
  println(35.isCoprimeTo(64))

  println("\nP34 (**) Calculate Euler's totient function phi(m)")
  println(10.totient)

  println("\nP35 (**) Determine the prime factors of a given positive integer")
  println(315.primeFactors)

  println("\nP36 (**) Determine the prime factors of a given positive integer (2)")
  println(315.primeFactorsMultiplicity)
}
