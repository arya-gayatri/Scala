package week4
import java.util.NoSuchElementException

object list {

  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]

  }

  class Nil[T] extends List[T] {

    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("nil.head")
    def tail: Nothing = throw new NoSuchElementException("nil.tail")

  }

  class Con[T](val head: T, val tail: List[T]) extends List[T] {

    def isEmpty: Boolean = false

  }

  def nth[T](n: Int, l: List[T]): T = {
    if(l.isEmpty) throw new IndexOutOfBoundsException
    if (n == 0) l.head
    else nth(n - 1, l.tail)
  }

  val list = new Con(1, new Con(2, new Con(3, new Nil)))

  nth(2,list)
}

