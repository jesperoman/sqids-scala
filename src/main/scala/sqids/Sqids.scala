package sqids

import sqids.ListExtensions._

import scala.annotation.tailrec
import sqids.options.Alphabet
import sqids.options.Blocklist
import sqids.options.InvalidSqidsOptions
import sqids.options.SqidsOptions
trait Sqids {
  def encodeUnsafeString(numbers: Int*): String
  def encodeUnsafe(numbers: Int*): Sqid
  def encode(numbers: Int*): Either[SqidsError, Sqid]
  def encode(numbers: List[Int]): Either[SqidsError, Sqid]
  def decode(id: String): List[Int]
  def minValue: Int
  def maxValue: Int
  def alphabet: Alphabet
}

object Sqids {
  def forAlphabet(a: Alphabet): Either[InvalidSqidsOptions, Sqids] =
    SqidsOptions.default.withAlphabet(a).map(Sqids.apply)

  def withBlocklist(blocklist: Blocklist): Sqids =
    apply(
      SqidsOptions.default.withBlocklist(blocklist = blocklist)
    )

  def default: Sqids =
    StringSqids.default
      // apply(SqidsOptions.default)

  def apply(options: SqidsOptions): Sqids = {
    val _alphabet = options.alphabet.shuffle
    new Sqids {

      override def encodeUnsafe(numbers: Int*): Sqid = encode(numbers*) match {
        case Left(value) => throw value
        case Right(value) => value
      }

      override def encodeUnsafeString(numbers: Int*): String = encode(numbers*) match {
        case Left(error) => throw error
        case Right(value) => value.value
      }

      override def encode(numbers: Int*): Either[SqidsError, Sqid] =
        encode(numbers.toList)

      override def alphabet: Alphabet = options.alphabet

      override def encode(numbers: List[Int]): Either[SqidsError, Sqid] =
        encode(numbers, false)

      override def minValue: Int = 0

      override def maxValue: Int = Int.MaxValue

      override def decode(input: String): List[Int] =
        input.toList match {
          case Nil => List.empty
          case s if s.exists(c => !_alphabet.value.contains(c)) => List.empty
          case prefix :: id => getNumbers(prefix, id.mkString)
        }

      private def getNumbers(prefix: Char, id: String): List[Int] = {
        @tailrec
        def go(
          id: String,
          alphabet: Alphabet,
          acc: Vector[Int] = Vector.empty
        ): List[Int] =
          if (id.isEmpty) acc.toList
          else {
            val separator = alphabet.separator
            id.split(separator) match {
              case List(c) => (acc :+ alphabet.removeSeparator.toNumber(c)).toList
              case c :: next =>
                val newId = next.mkString(separator.toString)
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
      ): Either[SqidsError, Sqid] =
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
