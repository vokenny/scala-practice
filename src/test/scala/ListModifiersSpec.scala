import org.scalatest.FlatSpec

class ListModifiersSpec extends FlatSpec  {

  val fibonacciList: List[Int] = 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: Nil
  val palindromeList: List[Int] = List(1, 2, 3, 2, 1)
  val unflatList: List[Any] = List(List(1, 1), 2, List(3, List(5, 8)))
  val symbolsList: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  "List Modifier method last" should "return the last element in a given list" in {
    assert(ListModifiers.last(fibonacciList) == 8)
  }

  "List Modifier method penultimare" should "return the penultimate element in a given list" in {
    assert(ListModifiers.penultimate(fibonacciList) == 5)
  }

  "List Modifier method kth" should "return the kth element in a given list" in {
    assert(ListModifiers.kth(2, fibonacciList) == 2)
    assert(ListModifiers.kth(4, fibonacciList) == 5)
  }

  it should "return IllegalArgumentException with invalid kth parameter" in {
    assertThrows[IllegalArgumentException] {
      ListModifiers.kth(-1, fibonacciList)
    }
  }

  "List Modifier method length" should "return the kth element in a given list" in {
    assert(ListModifiers.length(fibonacciList) == 6)
  }

  it should "return count of 0 for an empty list" in {
    assert(ListModifiers.length(Nil) == 0)
  }

  "List Modifier method reverse" should "return the given list in reverse" in {
    assert(ListModifiers.reverse(fibonacciList) == List(8, 5, 3, 2, 1, 1))
  }

  it should "return count of 0 for an empty list" in {
    assertThrows[IllegalArgumentException] {
      ListModifiers.reverse(Nil)
    }
  }

  "List Modifier method isPalindrome" should "return true for a palindrom" in {
    assert(ListModifiers.isPalindrome(palindromeList))
  }

  it should "return false for a non-palindrome" in {
    assert(!ListModifiers.isPalindrome(fibonacciList))
  }

  it should "return count of 0 for an empty list" in {
    assertThrows[IllegalArgumentException] {
      ListModifiers.isPalindrome(Nil)
    }
  }

  "List Modifier method flatten" should "return flat list" in {
    assert(ListModifiers.flatten(unflatList) == fibonacciList)
  }

  "List Modifier method compress" should "return list with duplicates removed" in {
    assert(ListModifiers.compress(symbolsList) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "return an empty list for a input of empty list" in {
    assert(ListModifiers.compress(List()) == List())
  }
}
