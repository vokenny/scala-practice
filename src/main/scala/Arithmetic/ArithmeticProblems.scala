package Arithmetic

object ArithmeticProblems extends App {

  println("\nP31 (**) Determine whether a given integer number is prime")
  println(ArithmeticOperations.isPrime(89))
  println(ArithmeticOperations.isPrime(100))

  println("\nP32 (**) Determine the greatest common divisor of two positive integer numbers")
  println(ArithmeticOperations.gcd(-2354, 250))

  println("\nP33 (*) Determine whether two positive integer numbers are coprime")
  println(ArithmeticOperations.isCoprime(35, 64))
}
