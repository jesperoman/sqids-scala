package sqids

import scala.annotation.tailrec
import sqids.options.Alphabet
import sqids.options.Blocklist

final case class Sqid(
  value: String,
  alphabet: Alphabet,
  numbers: List[Int],
  partitioned: Boolean,
  originalAlphabet: Alphabet
) {
  override def toString = value
  def withNextnr(nr: Int) = append(alphabet.removeSeparator.toId(nr))
  def addSeparator = append(alphabet.separator.toString)
  def addPartitionOrSeparator(partition: String, shouldAddPartition: Boolean) =
    if (shouldAddPartition) append(partition)
    else addSeparator
  def append(s: String) = copy(value = value + s)
  def length = value.length
  def fillToMinLength(minLength: Int): Sqid =
    copy(value =
      value.head.toString +
        alphabet.value.take(minLength - length) +
        value.drop(1).take(length)
    )
  def shuffle = copy(alphabet = alphabet.shuffle)

  def blocked(blocklist: Blocklist) = blocklist.isBlocked(value)

  def handleBlocked(blocklist: Blocklist): Sqid =
    if (blocked(blocklist))
      val newNumbers =
        if (partitioned)
          numbers.head + 1 :: numbers.tail
        else
          0 :: numbers
      Sqid
        .fromNumbers(newNumbers, originalAlphabet, true)
        .handleBlocked(blocklist)
    else this

  def handleMinLength(minLength: Int): Sqid =
    if (length < minLength)
      if (!partitioned)
        Sqid
          .fromNumbers(0 :: numbers, originalAlphabet, true)
          .handleMinLength(minLength)
      else
        fillToMinLength(minLength)
    else this
}

object Sqid {
  def fromNumbers(
    numbers: List[Int],
    a: Alphabet,
    partitioned: Boolean
  ): Sqid = {
    val alphabet = a.rearrange(numbers)

    val sqid = Sqid(
      value = alphabet.prefix.toString,
      alphabet = alphabet.removePrefixAndPartition,
      numbers = numbers,
      partitioned = partitioned,
      originalAlphabet = a
    )

    @tailrec
    def go(
      numbers: List[Int],
      sqid: Sqid,
      first: Boolean = false
    ): Sqid =
      numbers match
        case Nil => sqid.copy(value = "")
        case List(nr) => sqid.withNextnr(nr)
        case nr :: next =>
          go(
            numbers = next,
            sqid = sqid
              .withNextnr(nr)
              .addPartitionOrSeparator(
                alphabet.partition.toString,
                first && partitioned
              )
              .shuffle
          )

    go(numbers = numbers, sqid = sqid, first = true)
  }
}
