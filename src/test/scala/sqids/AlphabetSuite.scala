package sqids

import munit.ScalaCheckSuite

class AlphabetSuite extends ScalaCheckSuite {
  test("simple") {
    val sqids = Sqids.forAlphabet(
      Alphabet("0123456789abcdef").toOption.get
    )

    val numbers = List(1, 2, 3)
    val id = "4d9fd2"

    assertEquals(sqids.encode(numbers), id)
    assertEquals(sqids.decode(id), numbers)
  }

  test("short") {
    val sqids = Sqids.forAlphabet(Alphabet("abcde").toOption.get)

    val numbers = List(1, 2, 3)
    assertEquals(sqids.decode(sqids.encode(numbers)), numbers)
  }

  test("long") {
    val sqids = Sqids.forAlphabet(
      Alphabet(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+|{}[];:'\"/?.>,<`~"
      ).toOption.get
    )
    val numbers = List(1, 2, 3)
    assertEquals(sqids.decode(sqids.encode(numbers)), numbers)
  }

}
