package sqids.general

import sqids.SplitKit._

import scala.annotation.tailrec
import sqids.general.options.Alphabet
import sqids.SqidsError
import sqids.general.options.InvalidSqidsOptions
import sqids.general.options.SqidsOptions
import sqids.general.options.Blocklist

trait Sqids[A] {
  def encodeUnsafeValue(numbers: Int*): List[A]
  def encodeUnsafe(numbers: Int*): Sqid[A]
  def encode(numbers: Int*): Either[SqidsError, Sqid[A]]
  def encode(numbers: List[Int]): Either[SqidsError, Sqid[A]]
  def decode(id: List[A]): List[Int]
  def minValue: Int
  def maxValue: Int
  def alphabet: Alphabet[A]
}

object Sqids {
  def forAlphabet(a: Alphabet[Char]): Either[InvalidSqidsOptions, Sqids[Char]] =
    SqidsOptions.default.withAlphabet(a).map(Sqids.apply)

  def withBlocklist(blocklist: Blocklist[Char]): Sqids[Char] =
    apply(
      SqidsOptions.default.withBlocklist(blocklist = blocklist)
    )

  def default: Sqids[Char] =
    apply(SqidsOptions.default)

  def apply[A](options: SqidsOptions[A]): Sqids[A] = {
    val _alphabet = options.alphabet.shuffle
    new Sqids[A] {

      override def encodeUnsafe(numbers: Int*): Sqid[A] = encode(numbers: _*) match {
        case Left(value) => throw value
        case Right(value) => value
      }

      override def encodeUnsafeValue(numbers: Int*): List[A] = encode(numbers: _*) match {
        case Left(error) => throw error
        case Right(value) => value.value
      }

      override def encode(numbers: Int*): Either[SqidsError, Sqid[A]] =
        encode(numbers.toList)

      override def alphabet: Alphabet[A] = options.alphabet

      override def encode(numbers: List[Int]): Either[SqidsError, Sqid[A]] =
        encode(numbers, false)

      override def minValue: Int = 0

      override def maxValue: Int = Int.MaxValue

      override def decode(input: List[A]): List[Int] =
        input match {
          case Nil => List.empty
          case s if s.exists(c => !_alphabet.value.contains(c)) => List.empty
          case prefix :: id => getNumbers(prefix, id)
        }

      private def getNumbers(prefix: A, id: List[A]): List[Int] = {
        @tailrec
        def go(
          id: List[A],
          alphabet: Alphabet[A],
          acc: Vector[Int] = Vector.empty
        ): List[Int] =
          if (id.isEmpty) acc.toList
          else {
            val separator = alphabet.separator
            id.spl(separator) match {
              case List(c) => (acc :+ alphabet.removeSeparator.toNumber(c)).toList
              case c :: next =>
                val newId = next.join(List(separator)).flatten
                go(newId, alphabet.shuffle, acc :+ alphabet.removeSeparator.toNumber(c))
              case Nil => acc.toList
            }
          }

        val (alphabet, partitionIndex) = {
          val offset = _alphabet.value.indexOf(prefix)
          val rearranged = _alphabet.rearrange(offset)
          val partition = rearranged.partition
          (rearranged.removePrefixAndPartition, id.indexOf(partition))
        }

        if (partitionIndex > 0 && partitionIndex < id.length - 1)
          go(id.drop(partitionIndex + 1), alphabet.shuffle)
        else
          go(id, alphabet)
      }

      private def encode(
        numbers: List[Int],
        partitioned: Boolean
      ): Either[SqidsError, Sqid[A]] =
        numbers match {
          case numbers if numbers.exists(i => i > maxValue || i < minValue) =>
            Left(
              SqidsError.OutOfRange(
                s"some nr is out of range: $numbers, max: $maxValue min: $minValue"
              )
            )
          case numbers =>
            Right(
              Sqid
                .fromNumbers(numbers, _alphabet, partitioned)
                .handleMinLength(options.minLength)
                .handleBlocked(options.blocklist)
            )
        }

    }
  }
}
