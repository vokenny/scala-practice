import scala.annotation.tailrec

object ListModifiers {

  //P01
  def last[A](list: List[A]): A = {
    //list.last

    @tailrec
    def lastTailRec(result: List[A]): A = {
      result match {
        case Nil => throw new NoSuchElementException
        case h :: Nil => h
        case _ :: tail => lastTailRec(tail)
      }
    }

    lastTailRec(list)
  }

  //P02
  def penultimate[A](list: List[A]): A = {
    //list.init.last

    @tailrec
    def penultimateTailRec(result: List[A]): A = {
      result match {
        case h :: _ :: Nil => h
        case _ :: tail => penultimateTailRec(tail)
        case _ => throw new NoSuchElementException
      }
    }

    penultimateTailRec(list)
  }

  //P03
  def kth[A](k: Int, list: List[A]): A = {
    //if (k >= 0 && k < list.size) list(k) else throw new IllegalArgumentException

    @tailrec
    def kthTailRec(count: Int, result: List[A]): A = {
      (count, result) match {
        case (0, h :: _) => h
        case (n, _) if n < 0 => throw new IllegalArgumentException
        case (_, Nil) => throw new IllegalArgumentException
        case (n, _ :: tail) => kthTailRec(n - 1, tail)
      }
    }

    kthTailRec(k, list)
  }

  //P04
  def length[A](list: List[A]): Int = {
    //list.length
    //list.size
    //list.foldLeft(0) { (count, _) => count + 1

    @tailrec
    def lengthTailRec(count: Int, result: List[A]): Int = {
      result match {
        case Nil => count
        case _ :: Nil => count + 1
        case _ :: tail => lengthTailRec(count + 1, tail)
      }
    }

    lengthTailRec(0, list)
  }

  //P05
  def reverse[A](list: List[A]): List[A] = {
    //list.reverse
    //list.foldLeft(List[A]()) { (result, h) =>  h :: result}

    @tailrec
    def reverseTailRec(l: List[A], result: List[A]): List[A] = {
      l match {
        case Nil => throw new IllegalArgumentException
        case h :: Nil => h :: result
        case h :: tail => reverseTailRec(tail, h :: result)
      }
    }

    reverseTailRec(list, Nil)
  }

  //P06
  def isPalindrome[A](list: List[A]): Boolean = {
    //list == list.reverse
    //val rev = list.foldLeft(List[A]()) { (result, h) =>  h :: result}
    //if (list == rev) true else false

    @tailrec
    def isPalindromeTailRec(l: List[A]): Boolean = {
      l match {
        case Nil => throw new IllegalArgumentException
        case h :: Nil => true
        case h :: t :: Nil => if (h == t) true else false
        case h :: tail =>
          if (h == l.last) isPalindromeTailRec(l.drop(1).dropRight(1))
          else false
      }
    }

    isPalindromeTailRec(list)
  }

  //P07
  def flatten[A](list: List[A]): List[Any] = {
    list flatMap {
      case l: List[_] => flatten(l)
      case e => List(e)
    }
  }

  //P08
  def compress[A](list: List[A]): List[A] = {
    //list.foldRight(List[A]()) { (h, r) =>
    //  if (r.isEmpty || r.head != h) h :: r
    //  else r
    //}

    @tailrec
    def compressTailRec(l: List[A], result: List[A]): List[A] = {
      l match {
        case Nil => result.reverse
        case h :: tail => compressTailRec(tail.dropWhile(_ == h), h :: result)
      }
    }

    compressTailRec(list, Nil)
  }

  //P09
  def pack[A](list: List[A]): List[List[A]] = {
    //if (list.isEmpty) List(List())
    //else {
    //  val (packed, next) = list span { _ == list.head }
    //  if (next == Nil) List(packed)
    //  else packed :: pack(next)
    //}

    @tailrec
    def packTailRec(l: List[A], result: List[List[A]]): List[List[A]] = {
      l match {
        case Nil => result.reverse
        case h :: tail => packTailRec(tail.dropWhile(_ == h), l.takeWhile(_ == h) :: result)
      }
    }

    packTailRec(list, Nil)
  }

  //P10
  def encode[A](list: List[A]): List[(Int, A)] = {
    //pack(list).map { e => (e.size, e.head) }

    @tailrec
    def encodeTailRec(l: List[A], result: List[(Int, A)]): List[(Int, A)] = {
      l match {
        case Nil => result.reverse
        case h :: tail => encodeTailRec(tail.dropWhile(_ == h), (l.takeWhile(_ == h).size, h) :: result)
      }
    }

    encodeTailRec(list, Nil)
  }
}
