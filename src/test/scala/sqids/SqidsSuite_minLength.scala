package sqids

import munit.Suite
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen

final class SqidsSuite_minLength extends ScalaCheckSuite {

  val sqids = Sqids(
    SqidsOptions.default.copy(minLength =
      SqidsOptions.default.alphabet.value.length
    )
  )

  test("simple") {

    val numbers = List(1, 2, 3);
    val id = "75JILToVsGerOADWmHlY38xvbaNZKQ9wdFS0B6kcMEtnRpgizhjU42qT1cd0dL"

    assertEquals(sqids.encode(numbers), id)
    assertEquals(sqids.decode(id), numbers)
  }

  test("incremental numbers") {

    val ids = Map(
      "jf26PLNeO5WbJDUV7FmMtlGXps3CoqkHnZ8cYd19yIiTAQuvKSExzhrRghBlwf" -> List(
        0,
        0
      ),
      "vQLUq7zWXC6k9cNOtgJ2ZK8rbxuipBFAS10yTdYeRa3ojHwGnmMV4PDhESI2jL" -> List(
        0,
        1
      ),
      "YhcpVK3COXbifmnZoLuxWgBQwtjsSaDGAdr0ReTHM16yI9vU8JNzlFq5Eu2oPp" -> List(
        0,
        2
      ),
      "OTkn9daFgDZX6LbmfxI83RSKetJu0APihlsrYoz5pvQw7GyWHEUcN2jBqd4kJ9" -> List(
        0,
        3
      ),
      "h2cV5eLNYj1x4ToZpfM90UlgHBOKikQFvnW36AC8zrmuJ7XdRytIGPawqYEbBe" -> List(
        0,
        4
      ),
      "7Mf0HeUNkpsZOTvmcj836P9EWKaACBubInFJtwXR2DSzgYGhQV5i4lLxoT1qdU" -> List(
        0,
        5
      ),
      "APVSD1ZIY4WGBK75xktMfTev8qsCJw6oyH2j3OnLcXRlhziUmpbuNEar05QCsI" -> List(
        0,
        6
      ),
      "P0LUhnlT76rsWSofOeyRGQZv1cC5qu3dtaJYNEXwk8Vpx92bKiHIz4MgmiDOF7" -> List(
        0,
        7
      ),
      "xAhypZMXYIGCL4uW0te6lsFHaPc3SiD1TBgw5O7bvodzjqUn89JQRfk2Nvm4JI" -> List(
        0,
        8
      ),
      "94dRPIZ6irlXWvTbKywFuAhBoECQOVMjDJp53s2xeqaSzHY8nc17tmkLGwfGNl" -> List(
        0,
        9
      )
    )

    ids.foreach { case (id, numbers) =>
      assertEquals(sqids.encode(numbers), id)
      assertEquals(sqids.decode(id), numbers)
    }

  }

  test("min lengths") {
    List(0, 1, 5, 10, SqidsOptions.default.alphabet.value.length).foreach(
      minLength =>
        List(
          List(sqids.minValue),
          List(0, 0, 0, 0, 0),
          List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
          List(100, 200, 300),
          List(1000, 2000, 3000),
          List(1000000),
          List(sqids.maxValue)
        ).foreach { numbers =>
          val sqids = Sqids(SqidsOptions.default.copy(minLength = minLength))
          val id = sqids.encode(numbers)
          assert(id.length >= minLength)
          assertEquals(sqids.decode(id), numbers)
        }
    )

  }

  test("out-of-range invalid min length") {
    interceptMessage[SqidsError.OutOfRange]("minLength cant be < 0") {
      Sqids(SqidsOptions.default.copy(minLength = -1))
    }
    interceptMessage[SqidsError.OutOfRange](
      "minLength cant be > alphabet length"
    ) {
      Sqids(
        SqidsOptions.default.copy(minLength =
          SqidsOptions.default.alphabet.value.length + 1
        )
      )
    }
  }

// test.fails("out-of-range invalid min length"){
// 	expect(
// 		new Sqids({
// 			minLength: -1
// 		})
// 	).rejects;

// 	expect(
// 		new Sqids({
// 			minLength: defaultOptions.alphabet.length + 1
// 		})
// 	).rejects;
// })

}
