package cats.effect.bi

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Async, Concurrent, ContextShift, ExitCase, Fiber, IO => BaseIO}
import cats.implicits._
import cats.{Bifunctor, Eval, Monad, MonadError, Now, SemigroupK}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object BiIO extends BiIOInstances {

  implicit def biIOOps[F[_], E, A](e: BiIO[E, A]): BiIOOps[E, A] =
    new BiIOOps[E, A](e) {}

  def raiseError[E](e: E): BiIO[E, INothing] = create(BaseIO.raiseError(CustomException(e)))

  def terminate(e: Throwable): BiIO[INothing, INothing] = create(BaseIO.raiseError(e))

  def pure[A](a: A): IO[A] = create(BaseIO.pure(a))

  def delay[A](a: => A): IO[A] = create(BaseIO.delay(a))

  def suspend[A](fa: => IO[A]): IO[A] = create(BaseIO.suspend(embed[INothing, A](fa)))

  def pureBi[E, A](a: A): BiIO[E, A] = create(BaseIO.pure(a))

  def delayBi[E, A](a: => A): BiIO[E, A] = create(BaseIO.delay(a))

  def suspendBi[E, A](fa: => BiIO[E, A]): BiIO[E, A] = create(BaseIO.suspend(embed(fa)))

  def fromIO[A](fa: BaseIO[A]): IO[A] = create(fa)

  def toEitherIO[E, A](fa: BiIO[E, A]): BaseIO[Either[E, A]] =
    embed[INothing, Either[E, A]](attemptBi(fa))

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): BiIO[INothing, A] = create(BaseIO.async(k))

  def asyncF[A](k: (Either[Throwable, A] => Unit) => IO[Unit]): IO[A] =
    create(BaseIO.asyncF(cb => embed[INothing, Unit](k(cb))))

  def asyncBi[E, A](k: (Either[E, A] => Unit) => Unit): BiIO[E, A] =
    create(
      BaseIO.async(cb =>
        k {
          case r @ Right(_) => cb(r.leftCast[Throwable])
          case Left(e) => cb(Left(CustomException(e)))
        }
      )
    )

  def asyncFBi[E, A](k: (Either[E, A] => Unit) => BiIO[E, Unit]): BiIO[E, A] =
    create(
      BaseIO.asyncF(cb =>
        embed(k {
          case r @ Right(_) => cb(r.leftCast[Throwable])
          case Left(e) => cb(Left(CustomException(e)))
        })
      )
    )

  def attemptBi[E, A](fa: BiIO[E, A]): IO[Either[E, A]] =
    BiIO.create(
      BiIO
        .embed(fa)
        .redeemWith(
          {
            case BiIO.CustomException(e) => BaseIO.pure(Left(e.asInstanceOf[E]))
            case e => BaseIO.raiseError(e)
          },
          a => BaseIO.pure(Right(a))
        )
    )

  def fromEither[E, A](e: Either[E, A]): BiIO[E, A] =
    e match {
      case Right(a) => pureBi(a)
      case Left(e) => raiseError(e)
    }

  def fromTry[A](t: Try[A]): IO[A] =
    t match {
      case Success(a) => pure(a)
      case Failure(e) => terminate(e)
    }

  def cancelable[A](k: (Either[Throwable, A] => Unit) => BiIO[INothing, Unit]): IO[A] =
    create(BaseIO.cancelable[A](cb => embed(k(cb))))

  def cancelableBi[E, A](k: (Either[E, A] => Unit) => BiIO[E, Unit]): BiIO[E, A] =
    create(
      BaseIO.cancelable[A](cb =>
        embed(k {
          case r @ Right(_) => cb(r.leftCast[Throwable])
          case Left(e) => cb(Left(CustomException(e)))
        })
      )
    )

  def fromFuture[A](f: BiIO[INothing, Future[A]])(implicit cs: ContextShift[IO]): IO[A] =
    create(BaseIO.fromFuture(embed(f))(contextShiftForBaseIO(cs)))

  val never: IO[INothing] =
    create(BaseIO.never)

  val cancelBoundary: IO[Unit] =
    create(BaseIO.cancelBoundary)

  val unit: IO[Unit] = pure(())

  def none[A]: IO[Option[A]] = pureBi(None)

  /**
   * Lifts an `Eval` into `IO`.
   *
   * This function will preserve the evaluation semantics of any
   * actions that are lifted into the pure `IO`.  Eager `Eval`
   * instances will be converted into thunk-less `IO` (i.e. eager
   * `IO`), while lazy eval and memoized will be executed as such.
   */
  def eval[A](fa: Eval[A]): IO[A] =
    fa match {
      case Now(a) => pure(a)
      case notNow => delay(notNow.value)
    }

  def contextShift[E](ec: ExecutionContext): ContextShift[BiIO[E, *]] =
    new ContextShift[BiIO[E, *]] {
      val cs = BaseIO.contextShift(ec)
      def shift: BiIO[E, Unit] = create(cs.shift)
      def evalOn[A](ec: ExecutionContext)(fa: BiIO[E, A]): BiIO[E, A] = create(cs.evalOn(ec)(embed(fa)))
    }

  private[bi] def contextShiftForBaseIO[E](implicit cs: ContextShift[BiIO[E, *]]): ContextShift[BaseIO] =
    new ContextShift[BaseIO] {
      def shift: BaseIO[Unit] = embed(cs.shift)
      def evalOn[A](ec: ExecutionContext)(fa: BaseIO[A]): BaseIO[A] = embed(cs.evalOn(ec)(create(fa)))
    }

  private[bi] type Base
  private[bi] trait Tag extends Any
  type Type[+E, +A] <: Base with Tag

  private[cats] def create[E, A](s: BaseIO[A]): BiIO[E, A] =
    s.asInstanceOf[Type[E, A]]

  private[cats] def embed[E, A](e: BiIO[E, A]): BaseIO[A] =
    e.asInstanceOf[BaseIO[A]]

  private[bi] case class CustomException[+E](e: E) extends Exception
}

sealed abstract private[bi] class BiIOOps[E, A](val bio: BiIO[E, A]) {

  def attempt: BiIO[E, Either[E, A]] =
    BiIO.create(
      BiIO
        .embed(bio)
        .redeemWith(
          {
            case BiIO.CustomException(e) => BaseIO.pure(Left(e.asInstanceOf[E]))
            case e => BaseIO.raiseError(e)
          },
          a => BaseIO.pure(Right(a))
        )
    )

  def attemptBi: IO[Either[E, A]] =
    BiIO.create(
      BiIO
        .embed(bio)
        .redeemWith(
          {
            case BiIO.CustomException(e) => BaseIO.pure(Left(e.asInstanceOf[E]))
            case e => BaseIO.raiseError(e)
          },
          a => BaseIO.pure(Right(a))
        )
    )

  def rethrowBi[EE, AA](implicit ev0: A <:< Either[EE, AA], ev1: E <:< INothing): BiIO[EE, AA] =
    BiIO.create(
      BiIO.embed(bio.map(a => ev0(a).leftMap(BiIO.CustomException(_)))).rethrow
    )

  def toBaseIO: BaseIO[A] = BiIO.embed(bio)

  def toEitherT: EitherT[BaseIO, E, A] = EitherT(BiIO.embed(attemptBi))

  def unsafeRunToEither(): Either[E, A] = BiIO.embed(attempt).unsafeRunSync()

  def unsafeRunSync(): A = BiIO.embed(bio).unsafeRunSync()

  private def asyncBiIO: AsyncBiIO[E] = new AsyncBiIO[E] {}

  def map[B](f: A => B): BiIO[E, B] = asyncBiIO.map(bio)(f)

  def flatMap[B](f: A => BiIO[E, B]): BiIO[E, B] = asyncBiIO.flatMap(bio)(f)

  def map2[B, Z](second: BiIO[E, B])(f: (A, B) => Z): BiIO[E, Z] = asyncBiIO.map2(bio, second)(f)
}

private[bi] trait MonadBiIO[E] extends Monad[BiIO[E, *]] {
  def pure[A](a: A): BiIO[E, A] = BiIO.pureBi(a)

  def flatMap[A, B](fa: BiIO[E, A])(f: A => BiIO[E, B]): BiIO[E, B] =
    BiIO.create(BiIO.embed(fa).flatMap(a => BiIO.embed(f(a))))

  def tailRecM[A, B](a: A)(f: A => BiIO[E, Either[A, B]]): BiIO[E, B] =
    BiIO.create(Monad[BaseIO].tailRecM(a)(a => BiIO.embed(f(a))))
}

private[bi] trait AsyncBiIO[E] extends Async[BiIO[E, *]] with MonadBiIO[E] {

  def raiseError[A](e: Throwable): BiIO[E, A] = BiIO.terminate(e)

  def handleErrorWith[A](fa: BiIO[E, A])(f: Throwable => BiIO[E, A]): BiIO[E, A] =
    BiIO.create(BiIO.embed(fa).handleErrorWith {
      case e @ BiIO.CustomException(_) => BaseIO.raiseError(e)
      case e => BiIO.embed(f(e))
    })

  def bracketCase[A, B](acquire: BiIO[E, A])(use: A => BiIO[E, B])(
    release: (A, ExitCase[Throwable]) => BiIO[E, Unit]
  ): BiIO[E, B] =
    BiIO.create(
      Ref.of[BaseIO, Option[E]](None).flatMap { ref =>
        BiIO
          .embed(BiIO.attemptBi(acquire): BiIO[INothing, Either[E, A]])
          .bracketCase {
            case Right(a) => BiIO.embed(BiIO.attemptBi(use(a)): BiIO[INothing, Either[E, B]])
            case l @ Left(_) => BaseIO.pure(l.rightCast[B])
          } {
            case (Left(_), _) => BaseIO.unit
            case (Right(a), ExitCase.Completed) =>
              BiIO.embed(BiIO.attemptBi(release(a, ExitCase.Completed)): BiIO[INothing, Either[E, Unit]]).flatMap {
                case Left(l) => ref.set(Some(l))
                case Right(_) => BaseIO.unit
              }
            case (Right(a), res) => BiIO.embed(BiIO.attemptBi(release(a, res)): BiIO[INothing, Either[E, Unit]]).void
          }
          .flatMap {
            case Right(b) =>
              ref.get.flatMap(_.fold(BaseIO.pure(b))(e => BaseIO.raiseError(BiIO.CustomException(e))))
            case Left(e) => BaseIO.raiseError(BiIO.CustomException(e))
          }
      }
    )

  def suspend[A](thunk: => BiIO[E, A]): BiIO[E, A] = BiIO.suspendBi(thunk)

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): BiIO[E, A] = BiIO.async(k)

  def asyncF[A](k: (Either[Throwable, A] => Unit) => BiIO[E, Unit]): BiIO[E, A] =
    BiIO.create(BaseIO.asyncF(cb => BiIO.embed(k(cb))))
}

abstract private[bi] class BiIOInstances extends BiIOInstancesLowPriority {
  implicit def catsEffectBiConcurrentForBiIO[E](implicit cs: ContextShift[BiIO[E, *]]): Concurrent[BiIO[E, *]] =
    new Concurrent[BiIO[E, *]] with AsyncBiIO[E] {
      implicit val contextShiftBaseIO: ContextShift[BaseIO] = BiIO.contextShiftForBaseIO

      protected def biFiber[A](fiber: Fiber[BaseIO, A]): Fiber[BiIO[E, *], A] =
        Fiber(BiIO.create(fiber.join), BiIO.create(fiber.cancel))

      def start[A](fa: BiIO[E, A]): BiIO[E, Fiber[BiIO[E, *], A]] =
        BiIO.create(Concurrent[BaseIO].start(BiIO.embed(fa)).map(biFiber))

      def racePair[A, B](
        fa: BiIO[E, A],
        fb: BiIO[E, B]
      ): BiIO[E, Either[(A, Fiber[BiIO[E, *], B]), (Fiber[BiIO[E, *], A], B)]] =
        BiIO.create(BaseIO.racePair(BiIO.embed(fa), BiIO.embed(fb)).map {
          case Left((a, fib)) => Left((a, biFiber(fib)))
          case Right((fib, b)) => Right((biFiber(fib), b))
        })

    }

  implicit val catsEffectBifunctorForBiIO: Bifunctor[BiIO] = new Bifunctor[BiIO] {
    def bimap[A, B, C, D](fab: BiIO[A, B])(f: A => C, g: B => D): BiIO[C, D] =
      BiIO.biIOOps(fab.attemptBi.map(_.bimap(f, g)): BiIO[INothing, Either[C, D]]).rethrowBi
  }

  implicit def catsEffectBiSemigroupKForBiIO[E]: SemigroupK[BiIO[E, *]] =
    new SemigroupK[BiIO[E, *]] {
      def combineK[A](x: BiIO[E, A], y: BiIO[E, A]): BiIO[E, A] = x.handleErrorWith(_ => y)
    }
}

abstract private[bi] class BiIOInstancesLowPriority extends BiIOInstancesLowPriority2 {
  implicit def catsEffectBiAsyncForBiIO[E]: Async[BiIO[E, *]] = new AsyncBiIO[E] {}
}

abstract private[bi] class BiIOInstancesLowPriority2 {
  implicit def catsEffectBiMonadErrorForBiIO[E]: MonadError[BiIO[E, *], E] =
    new MonadError[BiIO[E, *], E] with MonadBiIO[E] {

      def raiseError[A](e: E): BiIO[E, A] = BiIO.raiseError(e)

      def handleErrorWith[A](fa: BiIO[E, A])(f: E => BiIO[E, A]): BiIO[E, A] =
        BiIO.create(BiIO.embed(fa).handleErrorWith {
          case BiIO.CustomException(e) => BiIO.embed(f(e.asInstanceOf[E]))
          case t => BaseIO.raiseError(t)
        })
    }
}
