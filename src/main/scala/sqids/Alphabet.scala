package sqids

import scala.util.control.NoStackTrace
import scala.collection.mutable.ArraySeq
import scala.annotation.tailrec

final case class InvalidAlphabet(override val getMessage: String) extends RuntimeException with NoStackTrace

sealed abstract case class Alphabet(value: String) {
  def length = value.length
  def indexOf(c: Char) = value.indexOf(c)
  def prefix = value(0)
  def partition = value(1)
  def removePrefixAndPartition: Alphabet = new Alphabet(value.drop(2)) {}
  def removeSeparator: Alphabet = new Alphabet(value.take(value.length - 1)) {}
  def separator: Char = value.last

  def toId(num: Int): String = {
    @tailrec
    def go(num: Int, acc: List[Char]): String =
      if (num <= 0) acc.mkString
      else
        go(num / length, value(num % length) :: acc)

    go(num / length, List(value(num % length)))
  }

  def toNumber(id: String): Int =
    id.foldLeft(0) { case (acc, c) =>
      acc * length + indexOf(c)
    }

  def shuffle: Alphabet = {
    val iRange = 0 to value.length - 2
    val jRange = (1 to value.length - 1).reverse

    val result: ArraySeq[String] = value.split("").to[ArraySeq]
    iRange.zip(jRange).map { case (i, j) =>
      val r = (i * j + result(i).codePointAt(0) + result(
        j
      ).codePointAt(0)) % value.length
      val rChar = result(r)
      val iChar = result(i)
      result(i) = rChar
      result(r) = iChar
    }
    new Alphabet(result.mkString) {}
  }

  def getOffset(numbers: List[Int]): Int =
    numbers.zipWithIndex.foldLeft(numbers.length) { case (acc, (number, index)) =>
      value(number % value.length).toString
        .codePointAt(0) + index + acc
    } % value.length

  def rearrange(offset: Int): Alphabet =
    new Alphabet(
      value.slice(offset, value.length) +
        value.slice(0, offset)
    ) {}

  def rearrange(numbers: List[Int]): Alphabet =
    rearrange(getOffset(numbers))
}

object Alphabet {
  def apply(value: String): Either[InvalidAlphabet, Alphabet] =
    value match {
      case v if value.distinct.length != value.length =>
        Left(InvalidAlphabet("Alphabet must contain unique characters"))
      case v if value.length < 5 =>
        Left(InvalidAlphabet("Alphabet must contain more than 5 characters"))
      case v =>
        Right(new Alphabet(v) {})
    }

  def default: Alphabet =
    new Alphabet((('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).mkString) {}
}
