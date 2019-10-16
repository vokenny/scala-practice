import org.scalatest.FlatSpec

class ListModifiersSpec extends FlatSpec  {

  "List Modifier method last" should "return the last element in a given list" in {
    assert(ListModifiers.last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  "List Modifier method penultimare" should "return the penultimate element in a given list" in {
    assert(ListModifiers.penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  }

  "List Modifier method kth" should "return the kth element in a given list" in {
    assert(ListModifiers.kth(2, List(1, 1, 2, 3, 5, 8)) == 2)
    assert(ListModifiers.kth(4, List(1, 1, 2, 3, 5, 8)) == 5)
  }

  it should "return IllegalArgumentException with invalid kth parameter" in {
    assertThrows[IllegalArgumentException] {
      ListModifiers.kth(-1, List(1, 1, 2, 3, 5, 8))
    }
  }

  "List Modifier method length" should "return the kth element in a given list" in {
    assert(ListModifiers.length(List(1, 1, 2, 3, 5, 8)) == 6)
  }

  it should "return count of 0 for an empty list" in {
    assert(ListModifiers.length(Nil) == 0)
  }

  "List Modifier method reverse" should "return the given list in reverse" in {
    assert(ListModifiers.reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  }

  it should "return count of 0 for an empty list" in {
    assertThrows[IllegalArgumentException] {
      ListModifiers.reverse(Nil)
    }
  }
}
