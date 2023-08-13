package sqids.general.options

import scala.util.control.NoStackTrace

final case class InvalidSqidsOptions(override val getMessage: String)
  extends RuntimeException
  with NoStackTrace

sealed abstract case class SqidsOptions[A](
  alphabet: Alphabet[A],
  minLength: Int,
  blocklist: Blocklist[A]
) {
  def withBlocklist(blocklist: Blocklist[A]): SqidsOptions[A] = new SqidsOptions(
    alphabet,
    minLength,
    blocklist.filter(alphabet)
  ) {}

  def withAlphabet(alphabet: Alphabet[A]): Either[InvalidSqidsOptions, SqidsOptions[A]] = SqidsOptions.apply(
    alphabet,
    minLength,
    blocklist
  )

  def withMinLength(minLength: Int): Either[InvalidSqidsOptions, SqidsOptions[A]] = SqidsOptions.apply(
    alphabet,
    minLength,
    blocklist
  )
}

object SqidsOptions {
  def apply[A](
    alphabet: Alphabet[A],
    minLength: Int,
    blocklist: Blocklist[A]
  ): Either[InvalidSqidsOptions, SqidsOptions[A]] =
    if (minLength < 0)
      Left(InvalidSqidsOptions("minLength need to be > 0"))
    else if (minLength > alphabet.length)
      Left(InvalidSqidsOptions("minLength cant be larger than alphabet length"))
    else
      Right(
        new SqidsOptions(
          alphabet,
          minLength,
          blocklist.filter(alphabet)
        ) {}
      )

  def default: SqidsOptions[Char] = new SqidsOptions(
    alphabet = Alphabet.default,
    minLength = 0,
    blocklist = Blocklist.default
  ) {}
}
