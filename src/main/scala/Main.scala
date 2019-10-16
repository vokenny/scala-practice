object Main extends App {

  val fibonacciList: List[Int] = 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: Nil
  val palindromeList: List[Int] = List(1, 2, 3, 2, 1)
  val unflatList: List[Any] = List(List(1, 1), 2, List(3, List(5, 8)))

  println("\nP01 (*) Find the last element of a list")
  println(ListModifiers.last(fibonacciList))

  println("\nP02 (*) Find the last but one element of a list")
  println(ListModifiers.penultimate(fibonacciList))

  println("\nP03 (*) Find the Kth element of a list")
  println(ListModifiers.kth(2, fibonacciList))

  println("\nP04 (*) Find the number of elements of a list")
  println(ListModifiers.length(fibonacciList))

  println("\nP05 (*) Reverse a list")
  println(ListModifiers.reverse(fibonacciList))

  println("\nP06 (*) Find out whether a list is a palindrome")
  println(ListModifiers.isPalindrome(palindromeList))
  println(ListModifiers.isPalindrome(fibonacciList))

  println("\nP07 (**) Flatten a nested list structure")
  println(ListModifiers.flatten(unflatList))
}
