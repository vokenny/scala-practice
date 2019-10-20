import java.util.NoSuchElementException

import scala.annotation.tailrec
import scala.util.Random

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

  //P11
  def encodeModified[A](list: List[A]): List[Any] = {
    //encode(list).map { e => if (e._1 == 1) e._2 else e }

    @tailrec
    def encodeModTailRec(l: List[A], result: List[Any]): List[Any] = {
      l match {
        case Nil => result.reverse
        case h :: tail =>
          if (h != tail.head) encodeModTailRec(tail.dropWhile(_ == h), h :: result)
          else encodeModTailRec(tail.dropWhile(_ == h), (l.takeWhile(_ == h).size, h) :: result)
      }
    }

    encodeModTailRec(list, Nil)
  }

  //P12
  def decode[A](list: List[(Int, A)]): List[A] = {
    //list flatMap { e => List.fill(e._1)(e._2)}

    @tailrec
    def decodeTailRec(l: List[(Int, A)], result: List[A]): List[A] = {
      l match {
        case Nil => result.reverse
        case h :: tail => decodeTailRec(tail, List.fill(h._1)(h._2) ::: result)
      }
    }

    decodeTailRec(list, Nil)
  }

  //P13 - my solution is the same as my P10 solution because I already did it directly
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    //if (list.isEmpty) Nil
    //else {
    //  val (packed, next) = list span { _ == list.head }
    //  (packed.length, packed.head) :: encodeDirect(next)
    //}

    @tailrec
    def encodeTailRec(l: List[A], result: List[(Int, A)]): List[(Int, A)] = {
      l match {
        case Nil => result.reverse
        case h :: tail => encodeTailRec(tail.dropWhile(_ == h), (l.takeWhile(_ == h).size, h) :: result)
      }
    }

    encodeTailRec(list, Nil)
  }

  //P14
  def duplicate[A](list: List[A]): List[A] = {
    //list flatMap { e => List(e, e) }

    list.flatMap { e => List.fill(2)(e) }
  }

  //P15
  def duplicateN[A](num: Int, list: List[A]): List[A] = {
    list.flatMap { e => List.fill(num)(e) }
  }

  //P16
  def drop[A](num: Int, list: List[A]): List[A] = {
    list.zipWithIndex.filter(e => (e._2 + 1) % num != 0).map(_._1)

    //@tailrec
    //def dropTailRec(c: Int, curList: List[A], result: List[A]): List[A] = {
    //  (c, curList) match {
    //    case (_, Nil) => result.reverse
    //    case (1, _ :: tail) => dropTailRec(num, tail, result)
    //    case (_, h :: tail) => dropTailRec(c - 1, tail, h :: result)
    //  }
    //}

    //dropTailRec(num, list, Nil)
  }

  //P17
  def split[A](num: Int, list: List[A]): (List[A], List[A]) = {
    //list.splitAt(num)

    if (list.isEmpty) throw new IllegalArgumentException
    else (list.take(num), list.drop(num))

    //@tailrec
    //def splitTailRec(count: Int, remList: List[A], result: List[A]): (List[A], List[A]) = {
    //  (count, remList) match {
    //    case (c, _) if c < 0 => throw new IllegalArgumentException
    //    case (_, Nil) => (result.reverse, Nil)
    //    case (0, _) => (result.reverse, remList)
    //    case (_, h :: tail) => splitTailRec(count - 1, tail, h :: result)
    //  }
    //}

    //splitTailRec(num, list, Nil)
  }

  //P18
  def slice[A](indStart: Int, indEnd: Int, list: List[A]): List[A] = {
    //list.slice(indStart, indEnd)

    list.drop(indStart).take(indEnd - indStart)
  }

  //P19
  def rotate[A](num: Int, list: List[A]): List[A] = {
    num.sign match {
      case 1 => list.drop(num) ::: list.take(num)
      case 0 => list
      case -1 => list.takeRight(num.abs) ::: list.dropRight(num.abs)
    }

    //@tailrec
    //def rotate(c: Int, result: List[A]): List[A] = {
    //  //
    //  c.sign match {
    //    case 1 => rotate(c - 1, result.tail ::: result.head :: Nil)
    //    case 0 => result
    //    case -1 => rotate(c + 1, result.last :: result.init)
    //  }
    //}

    //rotate(num, list)
  }

  //P20
  def removeAt[A](k: Int, list: List[A]): (List[A], A) = {
    if (k < 0 || k >= list.size) throw new NoSuchElementException
    else (list.zipWithIndex.filter(e => e._2 != k).map(e => e._1), list(k))

    //list.splitAt(index) match {
    //  case (Nil, _) => throw new NoSuchElementException
    //  case (pre, Nil) => throw new NoSuchElementException
    //  case (pre, e :: post) => (pre ::: post, e)
    //}
  }

  //P21
  def insertAt[A](elem: A, index: Int, list: List[A]): List[A] = {
    //list.splitAt(index) match {
    //  case (pre, post) => pre ::: elem :: post
    //}

    if (index >= 0 && index <= list.size) {
      list.splitAt(index) match {
        case (Nil, _) => elem :: Nil
        case (pre, Nil) => pre ::: elem :: Nil
        case (pre, post) => pre ::: elem :: post
      }
    } else throw new IllegalArgumentException
  }

  //P22 - my solution is better as it can return ranges going backwards
  def range(start: Int, end: Int): List[Int] = {
    //List.range(start, end + 1)

    @tailrec
    def rangeTailRec(c: Int, e: Int, result: List[Int]): List[Int] = {
      val sign = c.sign

      sign match {
        case 1 => rangeTailRec(c - 1, e + 1, e :: result)
        case 0 => (end :: result).reverse
        case -1 => rangeTailRec(c + 1, e - 1, e :: result)
      }
    }

    rangeTailRec(end - start, start, Nil)
  }

  //P24
  def lotto(num: Int, max: Int): List[Int] = {
    List.fill(num)(Random.nextInt(max + 1))

    //@tailrec
    //def lottoTailRec(count: Int, result: List[Int]): List[Int] = {
    //  if (count <= 0) result else lottoTailRec(count - 1, Random.nextInt(max + 1) :: result)
    //}

    //lottoTailRec(num, Nil)
  }
}
