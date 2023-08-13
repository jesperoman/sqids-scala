package sqids

import scala.annotation.tailrec

// Since default behaviour in splitting strings is different from javascript, this had to be implemented
object ListExtensions {
  extension [A](list: Iterable[A])
    def spl(delimiter: A): List[List[A]] = list.foldRight(List(List.empty[A])) {
      case (a, acc) if a == delimiter => List() :: acc
      case (a, acc) => (a :: acc.head) :: acc.tail
    }
    def split2(l: Iterable[A]): List[List[A]] = {
      @tailrec
      def go(left: List[A], acc: List[List[A]]): List[List[A]] =
        left match {
          case Nil => acc
          case left if left.startsWith(l) => go(left.drop(l.iterator.size), List() :: acc)
          case head :: next => go(next, (head :: acc.head) :: acc.tail)
        }

      go(list.toList, List(List.empty)).reverse.map(_.reverse)
    }
    def includes(l: Iterable[A]): Boolean =
      list.sliding(l.size).exists(_ == l)

    def join(d: A): List[A] =
      list.tail.foldLeft(List(list.head))((acc, a) => acc ++ List(d, a))

  extension (s: String)
    def split(delimiter: Char): List[String] =
      s.spl(delimiter).map(_.mkString)
    def splitJS(delimiter: String): List[String] =
      s.split2(delimiter).map(_.mkString)
}
