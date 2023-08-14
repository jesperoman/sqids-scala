# sqids-scala
An implementation of [sqids](https://sqids.org/) for scala

## Basic usage
Create a `Sqids` instance:
```scala
scala> import sqids._
import sqids._

scala> val s = Sqids.default
val s: sqids.Sqids = sqids.Sqids$$anon$1@72bff23a

scala> s.encodeUnsafeString(1,2,3)
val res0: String = 8QRLaD

scala> s.decode("8QRLaD")
val res1: List[Int] = List(1, 2, 3)
```

## Custom options
```scala
scala> import sqids.options._
import sqids.options._

scala> import sqids._
import sqids._

scala> val s = (for {
     |   alphabet <- Alphabet("abcdefghij")
     |   minLength = 5
     |   blocklist = Blocklist(Set("ab","cd"))
     |   options <- SqidsOptions(alphabet, minLength, blocklist)
     | } yield Sqids(options)).toOption.get
     |
val s: sqids.Sqids = sqids.Sqids$$anon$1@501cb2db

scala> s.encode(0)
val res2: Either[sqids.SqidsError,sqids.Sqid] = Right(adcid)

scala> s.decode("adcid")
val res3: List[Int] = List(0)

```
