package cats.effect.bi

import cats.effect.laws.ConcurrentLaws
import cats.effect.laws.util.TestContext
import org.typelevel.discipline.Laws
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop
import cats.effect.ContextShift
import cats.effect.laws.discipline.ConcurrentTests
import cats.implicits._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO
import org.scalacheck.Gen
import cats.kernel.Eq
import cats.effect.laws.util.TestInstances
import TestInstances.eqThrowable
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.util.control.NonFatal

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
    new Eq[BiIO[E, A]] {
      def eqv(x: BiIO[E, A], y: BiIO[E, A]): Boolean =
        TestInstances.eqIO[Either[E, A]].eqv(BiIO.toEitherIO(x), BiIO.toEitherIO(y))
    }

  checkAllAsync(
    "BiIO[String, *]",
    { implicit ec =>
      implicit val cs: ContextShift[BiIO[String, *]] = BiIO.contextShift(ec)
      ConcurrentTests[BiIO[String, *]].concurrent[Int, Int, Int]
    }
  )
}
