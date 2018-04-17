package cats
package laws

import syntax.flatMap._

/**
  * Laws that must be obeyed by any `cats.ErrorControl`.
  */
trait ErrorControlLaws[F[_], G[_], E] {
  implicit def E: ErrorControl[F, G, E]
  implicit val F: MonadError[F, E] = E.monadErrorF
  implicit val G: Monad[G] = E.monadG

  def deriveHandleError[A](fa: F[A])
                       (f: E => A): IsEq[F[A]] =
    E.accept(E.intercept(fa)(f)) <-> F.handleError(fa)(f)

  def deriveAttempt[A](fa: F[A]): IsEq[F[Either[E, A]]]=
    E.accept(E.trial(fa)) <-> F.attempt(fa)

  def deriveEnsureOr[A](ga: G[A])
                    (error: A => E)
                    (predicate: A => Boolean): IsEq[F[A]] =
    F.ensureOr(E.accept(ga))(error)(predicate) <-> E.assureOr(ga)(error)(predicate)

  def gNeverHasErrors[A](ga: G[A], f: E => A): IsEq[G[A]] =
    E.intercept(E.accept(ga))(f) <-> ga

  def raiseErrorControlError[A](e: E, f: E => G[A]): IsEq[G[A]] =
    E.controlError(F.raiseError[A](e))(f) <-> f(e)

  def controlErrorPureIsPure[A](a: A, f: E => G[A]): IsEq[G[A]] =
    E.controlError(F.pure(a))(f) <-> G.pure(a)

  def raiseErrorIntercept[A](e: E, f: E => A): IsEq[G[A]] =
    E.intercept(F.raiseError[A](e))(f) <-> G.pure(f(e))

  def raiseErrorTrial[A](e: E): IsEq[G[Either[E, A]]] =
    E.trial(F.raiseError[A](e)) <-> G.pure(Left(e))

  def trialAbsolve[A](fa: F[A]): IsEq[F[A]] =
    E.absolve(E.trial(fa)) <-> fa

  def monadHomomorphismFlatMap[A](ga: G[A], f: A => G[A]): IsEq[F[A]] =
    E.accept(ga.flatMap(f)) <-> E.accept(ga).flatMap(a => E.accept(f(a)))

  def monadHomomorphismPure[A](a: A): IsEq[F[A]] =
    E.accept(G.pure(a)) <-> F.pure(a)
}

object ErrorControlLaws {
  def apply[F[_], G[_], E](implicit ev: ErrorControl[F, G, E]): ErrorControlLaws[F, G, E] =
    new ErrorControlLaws[F, G, E] { def E: ErrorControl[F, G, E] = ev }
}
