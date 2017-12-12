package cats
package laws

/**
 * Laws that must be obeyed by any `cats.InvariantMonoidal`.
 */
trait InvariantMonoidalLaws[F[_]] extends SemigroupalLaws[F] {
  override implicit def F: InvariantMonoidal[F]
  import cats.syntax.semigroupal._
  import cats.syntax.invariant._

  def invariantMonoidalLeftIdentity[A, B](fa: F[A], b: B): IsEq[F[A]] =
    F.lift(b).product(fa).imap(_._2)(a => (b, a)) <-> fa

  def invariantMonoidalRightIdentity[A, B](fa: F[A], b: B): IsEq[F[A]] =
    fa.product(F.lift(b)).imap(_._1)(a => (a, b)) <-> fa
}

object InvariantMonoidalLaws {
  def apply[F[_]](implicit i: InvariantMonoidal[F]): InvariantMonoidalLaws[F] =
    new InvariantMonoidalLaws[F] { def F: InvariantMonoidal[F] = i }
}
