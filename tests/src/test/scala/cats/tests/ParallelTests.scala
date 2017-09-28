package cats


import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyVector.ZipNonEmptyVector
import cats.data._
import cats.tests.CatsSuite
import org.scalatest.FunSuite
import cats.laws.discipline.{ApplicativeErrorTests, NonEmptyParallelTests, SerializableTests, ParallelTests => ParallelTypeclassTests}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.Discipline

class ParallelTests extends CatsSuite with ApplicativeErrorForEitherTest {


  test("ParTraversing Either should accumulate errors") {
    forAll { es: List[Either[String, Int]] =>
      val lefts = es.collect {
        case Left(e) => e
      }.foldMap(identity)

      (es.parSequence.fold(identity, i => Monoid[String].empty)) should === (lefts)
    }
  }

  test("ParTraverse identity should be equivalent to parSequence") {
    forAll { es: List[Either[String, Int]] =>
      (es.parTraverse(identity)) should === (es.parSequence)
    }
  }

  test("ParTraverse_ identity should be equivalent to parSequence_") {
    forAll { es: Set[Either[String, Int]] =>
      (Parallel.parTraverse_(es)(identity)) should === (Parallel.parSequence_(es))
    }
  }

  test("parAp accumulates errors in order") {
    val right: Either[String, Int => Int] = Left("Hello")
    Parallel.parAp(right)("World".asLeft) should === (Left("HelloWorld"))
  }

  test("parAp2 accumulates errors in order") {
    val plus = (_: Int) + (_: Int)
    val rightPlus: Either[String, (Int, Int) => Int] = Right(plus)
    Parallel.parAp2(rightPlus)("Hello".asLeft, "World".asLeft) should === (Left("HelloWorld"))
  }

  test("Kleisli with Either should accumulate errors") {
    val k1: Kleisli[Either[String, ?], String, Int] = Kleisli(s => Right(s.length))
    val k2: Kleisli[Either[String, ?], String, Int] = Kleisli(s => Left("Boo"))
    val k3: Kleisli[Either[String, ?], String, Int] = Kleisli(s => Left("Nope"))

    (List(k1,k2,k3).parSequence.run("Hello")) should === (Left("BooNope"))

  }

  test("WriterT with Either should accumulate errors") {
    val w1: WriterT[Either[String, ?], String, Int] = WriterT.lift(Left("Too "))
    val w2: WriterT[Either[String, ?], String, Int] = WriterT.lift(Left("bad."))

    ((w1,w2).parMapN(_ + _).value) should === (Left("Too bad."))

  }

  test("ParMap over NonEmptyList should be consistent with zip") {
    forAll { (as: NonEmptyList[Int], bs: NonEmptyList[Int], cs: NonEmptyList[Int]) =>
      (as, bs, cs).parMapN(_ + _ + _) should === (as.zipWith(bs)(_ + _).zipWith(cs)(_ + _))
    }
  }

  test("ParMap over NonEmptyVector should be consistent with zip") {
    forAll { (as: NonEmptyVector[Int], bs: NonEmptyVector[Int], cs: NonEmptyVector[Int]) =>
      (as, bs, cs).parMapN(_ + _ + _) should === (as.zipWith(bs)(_ + _).zipWith(cs)(_ + _))
    }
  }

  test("ParMap over List should be consistent with zip") {
    forAll { (as: List[Int], bs: List[Int], cs: List[Int]) =>
      val zipped = as.zip(bs).map {
        case (a, b) => a + b
      }.zip(cs).map {
        case (a, b) => a + b
      }

      (as, bs, cs).parMapN(_ + _ + _) should === (zipped)
    }
  }

  test("ParMap over Vector should be consistent with zip") {
    forAll { (as: Vector[Int], bs: Vector[Int], cs: Vector[Int]) =>
      val zipped = as.zip(bs).map {
        case (a, b) => a + b
      }.zip(cs).map {
        case (a, b) => a + b
      }

      (as, bs, cs).parMapN(_ + _ + _) should === (zipped)
    }
  }

  test("ParMap over Stream should be consistent with zip") {
    forAll { (as: Stream[Int], bs: Stream[Int], cs: Stream[Int]) =>
      val zipped = as.zip(bs).map {
        case (a, b) => a + b
      }.zip(cs).map {
        case (a, b) => a + b
      }

      (as, bs, cs).parMapN(_ + _ + _) should === (zipped)
    }
  }

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?], Int].parallel)
  checkAll("Parallel[OptionT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[OptionT[Either[String, ?], ?], Nested[Validated[String, ?], Option, ?], Int].parallel)
  checkAll("Parallel[EitherT[M, String, ?], Nested[F, Validated[String, ?], ?]]", ParallelTypeclassTests[EitherT[Either[String, ?], String, ?], Nested[Validated[String, ?], Validated[String, ?], ?], Int].parallel)
  checkAll("Parallel[EitherT[Option, String, ?], Nested[Option, Validated[String, ?], ?]]", ParallelTypeclassTests[EitherT[Option, String, ?], Nested[Option, Validated[String, ?], ?], Int].parallel)
  checkAll("Parallel[WriterT[M, Int, ?], WriterT[F, Int, ?]]", ParallelTypeclassTests[WriterT[Either[String, ?], Int, ?], WriterT[Validated[String, ?], Int, ?], Int].parallel)
  checkAll("NonEmptyParallel[Vector, ZipVector]", NonEmptyParallelTests[Vector, ZipVector, Int].nonEmptyParallel)
  checkAll("NonEmptyParallel[List, ZipList]", NonEmptyParallelTests[List, ZipList, Int].nonEmptyParallel)
  // Can't test Parallel here, as Applicative[ZipStream].pure doesn't terminate
  checkAll("Parallel[Stream, ZipStream]", NonEmptyParallelTests[Stream, ZipStream, Int].nonEmptyParallel)
  checkAll("NonEmptyParallel[NonEmptyVector, ZipNonEmptyVector]", NonEmptyParallelTests[NonEmptyVector, ZipNonEmptyVector, Int].nonEmptyParallel)
  checkAll("NonEmptyParallel[NonEmptyList, ZipNonEmptyList]", NonEmptyParallelTests[NonEmptyList, ZipNonEmptyList, Int].nonEmptyParallel)
  checkAll("Parallel[NonEmptyStream, OneAnd[ZipStream, ?]", ParallelTypeclassTests[NonEmptyStream, OneAnd[ZipStream, ?], Int].parallel)


  checkAll("Parallel[Id, Id]", ParallelTypeclassTests[Id, Id, Int].parallel)

  checkAll("NonEmptyParallel[NonEmptyList, ZipNonEmptyList]", SerializableTests.serializable(NonEmptyParallel[NonEmptyList, ZipNonEmptyList]))
  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", SerializableTests.serializable(Parallel[Either[String, ?], Validated[String, ?]]))

  {
    implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll("Parallel[KlesliT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[Kleisli[Either[String, ?], Int, ?], Kleisli[Validated[String, ?], Int, ?], Int].parallel)
  }


}

trait ApplicativeErrorForEitherTest extends FunSuite with Discipline {

  import cats.instances.either._
  import cats.instances.parallel._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.unit._
  import cats.instances.tuple._

  implicit def eqV[A: Eq, B: Eq]: Eq[Validated[A, B]] = cats.data.Validated.catsDataEqForValidated

  {
    implicit val parVal = Parallel.applicativeError[Either[String, ?], Validated[String, ?], String]

    checkAll("ApplicativeError[Validated[String, Int]]", ApplicativeErrorTests[Validated[String, ?], String].applicativeError[Int, Int, Int])
  }

  {
    implicit val parVal = Parallel[Either[String, ?], Validated[String, ?]].applicativeError

    checkAll("Parallel[Validated[String, Int]].applicativeError", ApplicativeErrorTests[Validated[String, ?], String].applicativeError[Int, Int, Int])
  }
}
