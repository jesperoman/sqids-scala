package sqids

final case class SqidsOptions(
    alphabet: Alphabet,
    minLength: Int,
    blocklist: Blocklist
)

object SqidsOptions {
  def default: SqidsOptions = SqidsOptions(
    alphabet = Alphabet.default,
    minLength = 0,
    blocklist = Blocklist.default
  )
}
