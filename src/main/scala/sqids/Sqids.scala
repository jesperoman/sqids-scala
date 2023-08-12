package sqids

import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NoStackTrace
import scala.annotation.tailrec
import java.util.StringTokenizer

trait Sqids {
  def encode(numbers: List[Int]): String
  def decode(id: String): List[Int]
  def minValue: Int
  def maxValue: Int
  def alphabet: Alphabet
}

object Sqids {
  def forAlphabet(a: Alphabet): Sqids =
    apply(SqidsOptions.default.copy(alphabet = a))

  def withBlocklist(blocklist: Blocklist): Sqids =
    apply(
      SqidsOptions.default.copy(blocklist = blocklist)
    )

  def default: Sqids =
    apply(SqidsOptions.default)

  def apply(options: SqidsOptions): Sqids = {
    val _alphabet = options.alphabet.shuffle
    if (options.minLength < 0)
      throw SqidsError.OutOfRange("minLength cant be < 0")
    if (options.minLength > options.alphabet.value.length)
      throw SqidsError.OutOfRange("minLength cant be > alphabet length")
    new Sqids {

      override def alphabet: Alphabet = options.alphabet

      override def encode(numbers: List[Int]): String = encode(numbers, false)

      override def minValue: Int = 0

      override def maxValue: Int = Int.MaxValue

      override def decode(input: String): List[Int] = {
        var id = input
        val ret = ArrayBuffer[Int]()

        if (id == "") ret
        else if (id.exists(c => !_alphabet.value.contains(c)))
          ret
        else {
          val prefix = id(0)
          val offset = _alphabet.value.indexOf(prefix)
          var alphabet = _alphabet.rearrange(offset)
          val partition = alphabet.partition

          alphabet = alphabet.removePrefixAndPartition
          id = id.drop(1)
          val partitionIndex = id.indexOf(partition)
          if (partitionIndex > 0 && partitionIndex < id.length - 1) {
            id = id.drop(partitionIndex + 1)
            alphabet = alphabet.shuffle
          }

          while (id.length > 0) {
            val separator = alphabet.separator
            val chunks = splitString(id, separator)
            if (chunks.length > 0) {
              val alphabetWithoutSeparator =
                alphabet.removeSeparator
              ret.append(alphabetWithoutSeparator.toNumber(chunks(0)))
              if (chunks.length > 1)
                alphabet = alphabet.shuffle
            }
            id = chunks.drop(1).mkString(separator.toString)
          }
        }
        ret.toList
      }

      private def encode(
        numbers: List[Int],
        partitioned: Boolean = false
      ): String = {
        if (numbers.exists(i => i > maxValue || i < minValue))
          throw SqidsError.OutOfRange(
            s"some nr is out of range: $numbers, max: $maxValue min: $minValue"
          )
        if (numbers.isEmpty) ""
        else {
          var alphabet = _alphabet.rearrange(numbers)

          val prefix = alphabet.prefix

          val partition = alphabet.partition

          alphabet = alphabet.removePrefixAndPartition

          val ret = ArrayBuffer[String](prefix.toString)

          numbers.zipWithIndex.foreach { case (num, index) =>
            val alphabetWithoutSeparator = alphabet.removeSeparator
            ret.append(alphabetWithoutSeparator.toId(num))
            if (index < numbers.length - 1) {
              val separator = alphabet.separator
              if (partitioned && index == 0)
                ret.append(partition.toString)
              else ret.append(separator.toString)
              alphabet = alphabet.shuffle
            }
          }

          var id = ret.mkString

          if (options.minLength > id.length) {
            if (!partitioned)
              id = encode(0 :: numbers, true)

            if (options.minLength > id.length)
              id = id.slice(0, 1) + alphabet.value.slice(
                0,
                options.minLength - id.length
              ) + id.slice(1, id.length)
          }

          if (options.blocklist.isBlocked(id)) {
            var newNumbers = numbers.to[ArrayBuffer]
            if (partitioned)
              if (newNumbers(0) + 1 > this.maxValue)
                throw new RuntimeException(
                  "Ran out of range checking against the blocklist"
                )
              else
                newNumbers(0) += 1
            else
              newNumbers.prepend(0)
            id = encode(newNumbers.toList, true)
          }
          id
        }
      }
    }
  }

  // The default String.split doesn't include last element if empty
  // which is needed
  private def splitString(str: String, delimiter: Char): List[String] =
    str
      .foldLeft[List[List[Char]]](List(List.empty)) {
        case (acc, c) if c == delimiter => List() :: acc
        case (head :: tail, c) => (c :: head) :: tail
      }
      .map(_.reverse.mkString)
      .reverse

  // private def toId(num: Int, alphabet: String): String = {
  //   @tailrec
  //   def go(num: Int, acc: List[Char]): String =
  //     if (num <= 0) acc.mkString
  //     else
  //       go(num / alphabet.length, alphabet(num % alphabet.length) :: acc)

  //   go(num / alphabet.length, List(alphabet(num % alphabet.length)))
  // }
}
