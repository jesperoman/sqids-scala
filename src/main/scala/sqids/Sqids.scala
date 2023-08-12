package sqids

import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NoStackTrace
import scala.annotation.tailrec
import java.util.StringTokenizer

sealed trait SqidsError extends RuntimeException with NoStackTrace

object SqidsError {
  final case class OutOfRange(override val getMessage: String)
      extends SqidsError

  final case class AlphabetTooSmall(override val getMessage: String)
      extends SqidsError

  final case class AlphabetNotUnique(override val getMessage: String)
      extends SqidsError
}

trait Sqids {
  def encode(numbers: List[Int]): String
  def decode(id: String): List[Int]
  def minValue: Int
  def maxValue: Int
  def alphabet: Alphabet
}

object Sqids {
  def withBlocklist(blocklist: Blocklist): Sqids = apply(
    SqidsOptions.default.copy(blocklist = blocklist)
  )

  def default: Sqids = apply(SqidsOptions.default)
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
        else {
          if (id.exists(c => !_alphabet.value.contains(c)))
            ret
          else {
            val prefix = id(0)
            val offset = _alphabet.value.indexOf(prefix)
            var alphabet = _alphabet.value
              .slice(offset, _alphabet.value.length)
              .concat(_alphabet.value.slice(0, offset))

            val partition = alphabet(1)
            alphabet = alphabet.drop(2)
            id = id.drop(1)
            val partitionIndex = id.indexOf(partition)
            if (partitionIndex > 0 && partitionIndex < id.length - 1) {
              id = id.drop(partitionIndex + 1)
              alphabet = shuffle(alphabet)
            }

            while (id.length > 0) {
              val separator = alphabet.last
              val chunks = splitString(id, separator)
              if (chunks.length > 0) {
                val alphabetWithoutSeparator =
                  alphabet.take(alphabet.length - 1)
                ret.append(toNumber(chunks(0), alphabetWithoutSeparator))
                if (chunks.length > 1) {
                  alphabet = shuffle(alphabet)
                }
              }
              id = chunks.drop(1).mkString(separator.toString)
            }
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
          var alphabet = _alphabet.rearrange(numbers).value

          val prefix = alphabet(0)

          val partition = alphabet(1)

          alphabet = alphabet.drop(2)

          val ret = ArrayBuffer[String](prefix.toString)
          numbers.zipWithIndex.foreach { case (num, index) =>
            val alphabetWithoutSeparator = alphabet.take(alphabet.length - 1)
            ret.append(toIdRecursive(num, alphabetWithoutSeparator))
            if (index < numbers.length - 1) {
              val separator = alphabet.last
              if (partitioned && index == 0) {
                ret.append(partition.toString)
              } else ret.append(separator.toString)
              alphabet = shuffle(alphabet)
            }
          }

          var id = ret.mkString

          if (options.minLength > id.length) {
            if (!partitioned) {
              id = encode(0 :: numbers, true)
            }

            if (options.minLength > id.length) {
              id = id.slice(0, 1) + alphabet.slice(
                0,
                options.minLength - id.length
              ) + id.slice(1, id.length)
            }
          }

          if (options.blocklist.isBlocked((id))) {
            var newNumbers = numbers.to[ArrayBuffer]
            if (partitioned) {
              if (newNumbers(0) + 1 > this.maxValue) {
                throw new RuntimeException(
                  "Ran out of range checking against the blocklist"
                )
              } else {
                newNumbers(0) += 1
              }
            } else {
              newNumbers.prepend(0)
            }
            id = encode(newNumbers.toList, true)
          }
          id
        }
      }

    }

  }

  private def splitString(str: String, delimiter: Char): List[String] = {
    str
      .foldLeft[List[List[Char]]](List(List.empty)) {
        case (acc, c) if c == delimiter => List() :: acc
        case (head :: tail, c)          => (c :: head) :: tail
      }
      .map(_.reverse.mkString)
      .reverse
  }
  private def toNumber(id: String, alphabet: String): Int = {
    id.foldLeft(0) { case (acc, c) =>
      acc * alphabet.length + alphabet.indexOf(c)
    }
  }
  // private def isBlockedId(id: String, blocked: Set[String]): Boolean = {
  //   val lowerId = id.toLowerCase()
  //   println(s"checking $lowerId against ${blocked.size} blocked words")
  //   blocked
  //     .filterNot(_.length < lowerId.length)
  //     .map(_.toLowerCase())
  //     .exists(blockWord =>
  //       lowerId == blockWord ||
  //         (blockWord.matches("""\d""") && (lowerId
  //           .startsWith(blockWord) || lowerId.endsWith(blockWord))) ||
  //         lowerId.contains(blockWord)
  //     )
  // }
  private def toId(num: Int, alphabet: String): String = {
    val id = ArrayBuffer[Char]()
    var result = num
    do {
      id.prepend(
        alphabet(result % alphabet.length)
      )
      result = result / alphabet.length
    } while (result > 0)
    id.mkString
  }

  private def toIdRecursive(num: Int, alphabet: String): String = {
    @tailrec
    def loop(num: Int, acc: List[Char]): String = {
      if (num <= 0) acc.mkString
      else {
        loop(num / alphabet.length, alphabet(num % alphabet.length) :: acc)
      }
    }
    loop(num / alphabet.length, List(alphabet(num % alphabet.length)))
  }

  private def shuffle(alphabet: String): String = {
    val iRange = (0 to alphabet.length - 2)
    val jRange = (1 to alphabet.length - 1).reverse

    val result: ArraySeq[String] = alphabet.split("").to[ArraySeq]
    iRange.zip(jRange).map { case (i, j) =>
      val r = (i * j + result(i).codePointAt(0) + result(
        j
      ).codePointAt(0)) % alphabet.length
      val rChar = result(r)
      val iChar = result(i)
      result(i) = rChar
      result(r) = iChar
    }
    result.mkString
  }
}
