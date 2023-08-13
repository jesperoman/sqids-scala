package sqids.general.options

import scala.util.control.NoStackTrace
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

final case class InvalidAlphabet(override val getMessage: String) extends RuntimeException with NoStackTrace

sealed abstract case class Alphabet[A](value: List[A]) {
  def length = value.length
  def indexOf(a: A) = value.indexOf(a)
  def prefix = value.head
  def partition = value(1)
  def removePrefixAndPartition: Alphabet[A] = new Alphabet(value.drop(2)) {}
  def removeSeparator: Alphabet[A] = new Alphabet(value.take(value.length - 1)) {}
  def separator: A = value.last

  def toId(num: Int): List[A] = {
    @tailrec
    def go(num: Int, acc: List[A]): List[A] =
      if (num <= 0) acc
      else
        go(num / length, value(num % length) :: acc)

    go(num / length, List(value(num % length)))
  }

  def toNumber(id: List[A]): Int =
    id.foldLeft(0) { case (acc, c) =>
      acc * length + indexOf(c)
    }

  def shuffle: Alphabet[A] = {
    val iRange = 0 to value.length - 2
    val jRange = (1 to value.length - 1).reverse

    val result: ArrayBuffer[A] = ArrayBuffer.from(value)
    iRange.zip(jRange).foreach { case (i, j) =>
      val r = (i *
        j +
        result(i).toString
          .codePointAt(0) +
        result(j).toString
          .codePointAt(0)) %
        value.length
      val rChar = result(r)
      val iChar = result(i)
      result(i) = rChar
      result(r) = iChar
    }
    new Alphabet(result.toList) {}
  }

  def getOffset(numbers: List[Int]): Int =
    numbers.zipWithIndex
      .foldLeft(numbers.length) { case (acc, (number, index)) =>
        value(number % value.length).toString
          .codePointAt(0) + index + acc
      } % value.length

  def rearrange(offset: Int): Alphabet[A] =
    new Alphabet(value.drop(offset) ++ value.take(offset)) {}

  def rearrange(numbers: List[Int]): Alphabet[A] =
    rearrange(getOffset(numbers))
}

object Alphabet {
  def apply[A](value: List[A]): Either[InvalidAlphabet, Alphabet[A]] =
    value match {
      case v if v.distinct.length != v.length =>
        Left(InvalidAlphabet("Alphabet must contain unique elements"))
      case v if v.length < 5 =>
        Left(InvalidAlphabet("Alphabet must contain more or equal to 5 elements"))
      case v =>
        Right(new Alphabet(v) {})
    }

  def default: Alphabet[Char] =
    new Alphabet((('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toList) {}
}
