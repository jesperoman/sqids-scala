package sqids

import munit.ScalaCheckSuite

class AlphabetSuite extends ScalaCheckSuite {
  test("simple") {
    val numbers = List(1, 2, 3)
    val id = "4d9fd2"
    Alphabet("0123456789abcdef")
      .map(Sqids.forAlphabet)
      .foreach { sqids =>
        assertEquals(sqids.encode(numbers), id)
        assertEquals(sqids.decode(id), numbers)
      }
  }

  test("short alphabet") {
    val numbers = List(1, 2, 3)
    Alphabet("abcde")
      .map(Sqids.forAlphabet)
      .foreach(sqids => assertEquals(sqids.decode(sqids.encode(numbers)), numbers))
  }

  test("long alphabet") {
    val numbers = List(1, 2, 3)
    Alphabet(
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+|{}[];:'\"/?.>,<`~"
    )
      .map(Sqids.forAlphabet)
      .foreach(sqids => assertEquals(sqids.decode(sqids.encode(numbers)), numbers))
  }

  test("repeating alphabet characters") {
    assert(Alphabet("aabcdefg").isLeft)
  }

  test("too short of an alphabet") {
    assert(Alphabet("abcd").isLeft)
  }
}
