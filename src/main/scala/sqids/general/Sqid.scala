package sqids.general

import scala.annotation.tailrec
import sqids.general.options.Alphabet
import sqids.general.options.Blocklist

final case class Sqid[A](
  value: List[A],
  alphabet: Alphabet[A],
  numbers: List[Int],
  partitioned: Boolean,
  originalAlphabet: Alphabet[A]
) {
  override def toString = value.mkString("-")
  def withNextnr(nr: Int) = append(alphabet.removeSeparator.toId(nr))
  def addSeparator = append(List(alphabet.separator))
  def addPartitionOrSeparator(partition: A, shouldAddPartition: Boolean) =
    if (shouldAddPartition) append(List(partition))
    else addSeparator
  def append(s: List[A]) = copy(value = value ++ s)
  def length = value.length
  def fillToMinLength(minLength: Int): Sqid[A] =
    copy(value =
      List(value.head) ++
        alphabet.value.take(minLength - length) ++
        value.drop(1).take(length)
    )
  def shuffle = copy(alphabet = alphabet.shuffle)

  def blocked(blocklist: Blocklist[A]) = blocklist.isBlocked(value)

  def handleBlocked(blocklist: Blocklist[A]): Sqid[A] =
    if (blocked(blocklist)) {
      val newNumbers =
        if (partitioned)
          numbers.head + 1 :: numbers.tail
        else
          0 :: numbers
      Sqid
        .fromNumbers(newNumbers, originalAlphabet, true)
        .handleBlocked(blocklist)
    } else this

  def handleMinLength(minLength: Int): Sqid[A] =
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
  def fromNumbers[A](
    numbers: List[Int],
    a: Alphabet[A],
    partitioned: Boolean
  ): Sqid[A] = {
    val alphabet = a.rearrange(numbers)

    val sqid = Sqid(
      value = List(alphabet.prefix),
      alphabet = alphabet.removePrefixAndPartition,
      numbers = numbers,
      partitioned = partitioned,
      originalAlphabet = a
    )

    @tailrec
    def go(
      numbers: List[Int],
      sqid: Sqid[A],
      first: Boolean = false
    ): Sqid[A] =
      numbers match {
        case Nil => sqid.copy(value = List.empty)
        case List(nr) => sqid.withNextnr(nr)
        case nr :: next =>
          go(
            numbers = next,
            sqid = sqid
              .withNextnr(nr)
              .addPartitionOrSeparator(
                alphabet.partition,
                first && partitioned
              )
              .shuffle
          )
      }

    go(numbers = numbers, sqid = sqid, first = true)
  }
}
