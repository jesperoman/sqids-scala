package sqids

import sqids.general.{Sqids => GenSqids}
import sqids.options.Alphabet
import sqids.Sqids

object StringSqids {
  def default: Sqids = {
    val charsSqids = GenSqids.default
    val a = Alphabet(charsSqids.alphabet.value.mkString) match
      case Left(value) => throw value
      case Right(value) => value

    new Sqids {
      override def maxValue: Int = charsSqids.maxValue
      override def encode(numbers: List[Int]): Either[SqidsError, Sqid] =
        charsSqids
          .encode(numbers)
          .flatMap(genSqid =>
            Alphabet(genSqid.alphabet.value.mkString)
              .map(a =>
                Sqid(
                  value = genSqid.value.mkString,
                  alphabet = a,
                  numbers = genSqid.numbers,
                  partitioned = genSqid.partitioned,
                  originalAlphabet = alphabet
                )
              )
              .left
              .map(e =>
                SqidsError.EncodeError(s"Error translating alphabet for some reason: ${e.getMessage}")
              )
          )

      override def encode(numbers: Int*): Either[SqidsError, Sqid] = encode(numbers.toList)

      override def decode(id: String): List[Int] = charsSqids.decode(id.toList)

      override def encodeUnsafe(numbers: Int*): Sqid = encode(numbers*) match {
        case Left(error) => throw error
        case Right(value) => value
      }

      override def minValue: Int = charsSqids.minValue

      override def alphabet: Alphabet = a

      override def encodeUnsafeString(numbers: Int*): String = encodeUnsafe(numbers*).value

    }

  }
}
