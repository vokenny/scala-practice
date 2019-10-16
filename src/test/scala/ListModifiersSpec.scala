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
}
