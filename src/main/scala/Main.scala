object Main extends App {

  val fibonacciList: List[Int] = 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: Nil
  val palindromeList: List[Int] = List(1, 2, 3, 2, 1)
  val unflatList: List[Any] = List(List(1, 1), 2, List(3, List(5, 8)))
  val symbolsList: List[Symbol] = List(Symbol("a"), Symbol("a"), Symbol("a"), Symbol("a"),  Symbol("b"), Symbol("c"), Symbol("c"), Symbol("a"), Symbol("a"), Symbol("d"), Symbol("e"), Symbol("e"), Symbol("e"), Symbol("e"))
  val encodedList = List((4, Symbol("a")), (1, Symbol("b")), (2, Symbol("c")), (2, Symbol("a")), (1, Symbol("d")), (4, Symbol("e")))
  val abridgeAlphabetList = List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"), Symbol("e"), Symbol("f"), Symbol("g"), Symbol("h"), Symbol("i"), Symbol("j"), Symbol("k"))

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

  println("\nP08 (**) Eliminate consecutive duplicates of list elements")
  println(ListModifiers.compress(symbolsList))

  println("\nP09 (**) Pack consecutive duplicates of list elements into sublists")
  println(ListModifiers.pack(symbolsList))

  println("\nP10 (*) Run-length encoding of a list")
  println(ListModifiers.encode(symbolsList))

  println("\nP11 (*) Modified run-length encoding")
  println(ListModifiers.encodeModified(symbolsList))

  println("\nP12 (**) Decode a run-length encoded list")
  println(ListModifiers.decode(encodedList))

  println("\nP13 (**) Run-length encoding of a list (direct solution)")
  println(ListModifiers.encodeDirect(symbolsList))

  println("\nP14 (*) Duplicate the elements of a list")
  println(ListModifiers.duplicate(abridgeAlphabetList))

  println("\nP15 (**) Duplicate the elements of a list a given number of times")
  println(ListModifiers.duplicateN(3, abridgeAlphabetList))

  println("\nP24 (*) Lotto: Draw N different random numbers from the set 1..M")
  println(ListModifiers.lotto(6, 49))
}
