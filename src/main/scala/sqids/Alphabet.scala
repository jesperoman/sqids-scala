package sqids

import scala.util.control.NoStackTrace
import scala.collection.mutable.ArraySeq

final case class InvalidAlphabet(override val getMessage: String)
    extends RuntimeException
    with NoStackTrace

sealed abstract case class Alphabet(value: String) {
  def shuffle: Alphabet = {
    val iRange = (0 to value.length - 2)
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
    numbers.zipWithIndex.foldLeft(numbers.length) {
      case (acc, (number, index)) =>
        value(number % value.length).toString
          .codePointAt(0) + index + acc
    } % value.length

  def rearrange(numbers: List[Int]): Alphabet = {
    val offset = getOffset(numbers)
    new Alphabet(
      value.slice(offset, value.length) +
        value.slice(0, offset)
    ) {}
  }
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
