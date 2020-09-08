package cats.effect.bi

import cats.effect.ContextShift
import cats.effect.bi.BiIO.{pure, raiseError, terminate}
import cats.effect.laws.discipline.ConcurrentTests
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.effect.laws.util.TestInstances.eqThrowable
import cats.implicits._
import cats.kernel.Eq
import cats.laws.discipline.{BifunctorTests, MonadErrorTests, SemigroupKTests}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.typelevel.discipline.Laws

class BiIOSuite extends munit.DisciplineSuite {
  def checkAllAsync(name: String, f: TestContext => Laws#RuleSet): Unit = {
    val context = TestContext()
    val ruleSet = f(context)

    for ((id, prop) <- ruleSet.all.properties)
      property(name + "." + id)(prop)
  }

  implicit def arbBiIO[E: Arbitrary, A: Arbitrary: Cogen]: Arbitrary[BiIO[E, A]] =
    Arbitrary(
      Gen.frequency(
        3 -> catsEffectLawsArbitraryForIO[A].arbitrary.map(BiIO.fromIO),
        2 -> Arbitrary.arbitrary[E].map(e => BiIO.raiseError(e))
      )
    )

  implicit def eqBiIO[E: Eq, A: Eq](implicit ec: TestContext): Eq[BiIO[E, A]] =
    (x: BiIO[E, A], y: BiIO[E, A]) => TestInstances.eqIO[Either[E, A]].eqv(BiIO.toEitherIO(x), BiIO.toEitherIO(y))

  checkAllAsync(
    "BiIO[String, *]",
    { implicit ec =>
      implicit val cs: ContextShift[BiIO[String, *]] = BiIO.contextShift(ec)
      ConcurrentTests[BiIO[String, *]].concurrent[Int, Int, Int]
    }
  )

  checkAllAsync(
    "BiIO",
    { implicit ec =>
      BifunctorTests[BiIO].bifunctor[Int, Int, Int, Int, Int, Int]
    }
  )

  checkAllAsync(
    "BiIO[String, *]",
    { implicit ec =>
      MonadErrorTests[BiIO[String, *], String].monadError[Int, Int, Int]
    }
  )

  checkAllAsync(
    "BiIO[String, *]",
    { implicit ec =>
      SemigroupKTests[BiIO[String, *]].semigroupK[Int]
    }
  )

  test("IO <-> BiIO type inference") {
    val x: BiIO[INothing, Int] = pure(5)
    val y: IO[Int] = x

    val xx: IO[Int] = pure(5)
    val yy: BiIO[INothing, Int] = xx
  }

  test("Left Right combined inference") {
    val pureValue: IO[Int] = pure(5)
    val errValue: BiIO[RuntimeException, INothing] = raiseError(new RuntimeException)

    val combined1: BiIO[RuntimeException, Int] = pureValue
    val combined2: BiIO[RuntimeException, Int] = errValue
  }

  test("Recovery") {
    val ioa: BiIO[INothing, Int] = terminate(new RuntimeException())

    // todo: need companion object
    import BiIO._
    assertEquals(ioa.recover { case _ => 5 }.unsafeRunSync(), 5)
  }
}
