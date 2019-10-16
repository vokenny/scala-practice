object Main extends App {

  val fibonacciList: List[Int] = 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: Nil

  println("P01 (*) Find the last element of a list")
  println(ListModifiers.last(fibonacciList))

  println("P02 (*) Find the last but one element of a list")
  println(ListModifiers.penultimate(fibonacciList))

  println("P03 (*) Find the Kth element of a list")
  println(ListModifiers.kth(2, fibonacciList))

  println("P04 (*) Find the number of elements of a list")
  println(ListModifiers.length(fibonacciList))

  println("P05 (*) Reverse a list")
  println(ListModifiers.reverse(fibonacciList))
}
