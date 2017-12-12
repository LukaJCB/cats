package cats

import simulacrum.typeclass

/**
 * [[ContravariantSemigroupal]] is nothing more than something both contravariant
 * and Semigroupal. It comes up enough to be useful, and composes well
 * Also sometimes known as `Divide`.
 */
@typeclass trait ContravariantSemigroupal[F[_]] extends Semigroupal[F] with Contravariant[F] { self =>
  override def composeFunctor[G[_]: Functor]: ContravariantSemigroupal[λ[α => F[G[α]]]] =
    new ComposedSemigroupal[F, G] {
      def F = self
      def G = Functor[G]
    }

  override def semigroup[A](implicit A: Semigroup[A]): Semigroup[F[A]] =
    new ContravariantSemigroupalSemigroup[F, A](this)
}

object ContravariantSemigroupal extends SemigroupalArityFunctions {
  def semigroup[F[_], A](implicit f: ContravariantSemigroupal[F]): Semigroup[F[A]] =
    new ContravariantSemigroupalSemigroup[F, A](f)
}

private[cats] class ContravariantSemigroupalSemigroup[F[_], A](f: ContravariantSemigroupal[F]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    ContravariantSemigroupal.contramap2(a, b)((a: A) => (a, a))(f, f)
}
