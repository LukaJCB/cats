package cats.instances

import cats.data.{EitherT, Nested, OptionT, Validated}
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{Applicative, Functor, Monad, Parallel, ~>}


trait ParallelInstances {
  implicit def catsParallelForEitherValidated[E: Semigroup]: Parallel[Either[E, ?], Validated[E, ?]] = new Parallel[Either[E, ?], Validated[E, ?]] {
    def applicative: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated

    def sequential(implicit M: Monad[Either[E, ?]]): Validated[E, ?] ~> Either[E, ?] =
      λ[Validated[E, ?] ~> Either[E, ?]](_.toEither)

    def parallel(implicit M: Monad[Either[E, ?]]): Either[E, ?] ~> Validated[E, ?] =
      λ[Either[E, ?] ~> Validated[E, ?]](_.toValidated)
  }

  implicit def catsParallelForOptionTNestedOption[F[_], M[_]: Monad]
  (implicit P: Parallel[M, F]): Parallel[OptionT[M, ?], Nested[F, Option, ?]] = new Parallel[OptionT[M, ?], Nested[F, Option, ?]] {
    implicit val appF: Applicative[F] = P.applicative
    implicit val appOption: Applicative[Option] = cats.instances.option.catsStdInstancesForOption

    def applicative: Applicative[Nested[F, Option, ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Option]

    def sequential(implicit M: Monad[OptionT[M, ?]]): Nested[F, Option, ?] ~> OptionT[M, ?] =
      λ[Nested[F, Option, ?] ~> OptionT[M, ?]](nested => OptionT(P.sequential.apply(nested.value)))

    def parallel(implicit M: Monad[OptionT[M, ?]]): OptionT[M, ?]~> Nested[F, Option, ?] =
      λ[OptionT[M, ?] ~> Nested[F, Option, ?]](optT => Nested(P.parallel.apply(optT.value)))
  }

  implicit def catsParallelForEitherTNestedValidated[F[_], M[_]: Monad, E: Semigroup]
  (implicit P: Parallel[M, F]): Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]] =
    new Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]] {

    implicit val appF: Applicative[F] = P.applicative
    implicit val appValidated: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated

    def applicative: Applicative[Nested[F, Validated[E, ?], ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Validated[E, ?]]

    def sequential(implicit M: Monad[EitherT[M, E, ?]]): Nested[F, Validated[E, ?], ?] ~> EitherT[M, E, ?] =
      λ[Nested[F, Validated[E, ?], ?] ~> EitherT[M, E, ?]] { nested =>
        val mva = P.sequential.apply(nested.value)
        EitherT(Functor[M].map(mva)(_.toEither))
      }

    def parallel(implicit M: Monad[EitherT[M, E, ?]]): EitherT[M, E, ?]~> Nested[F, Validated[E, ?], ?] =
      λ[EitherT[M, E, ?] ~> Nested[F, Validated[E, ?], ?]] { eitherT =>
        val fea = P.parallel.apply(eitherT.value)
        Nested(Functor[F].map(fea)(_.toValidated))
      }
  }
}
