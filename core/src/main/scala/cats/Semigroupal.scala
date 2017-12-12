package cats

import simulacrum.typeclass

/**
  * [[Semigroupal]] Functors capture the idea of composing independent effectful values.
  * It is of particular interest when taken together with a covariant [[Functor]] - where [[Functor]]
  * captures the idea of applying a unary pure function to an effectful value,
  * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
  * independent effectful values.
  *
  * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
  * [[Semigroupal]] and [[Functor]] to illustrate this.
  *
  * [[Semigroupal]] is an nothing more than something both invariant
  * and Semigroupal.
  */
@typeclass trait Semigroupal[F[_]] extends Invariant[F] { self =>

   def composeApply[G[_]: Apply]: Semigroupal[λ[α => F[G[α]]]] =
     new ComposedApplySemigroupal[F, G] {
       def F = self
       def G = Apply[G]
     }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  /**
    * Gives a `Semigroup` instance if A itself has a `Semigroup` instance.
    */
  def semigroup[A](implicit A: Semigroup[A]): Semigroup[F[A]] =
    new InvariantSemigroupalSemigroup[F, A](this, A)

}

object Semigroupal extends SemigroupalArityFunctions

private[cats] class InvariantSemigroupalSemigroup[F[_], A](f: Semigroupal[F], sg: Semigroup[A]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    Semigroupal.imap2(a, b)(sg.combine)(a => (a, a))(f, f)
}
