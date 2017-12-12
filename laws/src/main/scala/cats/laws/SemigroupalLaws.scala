package cats
package laws


/**
  * Laws that are expected for any `cats.InvariantSemigroupal`.
  */
trait SemigroupalLaws[F[_]] extends InvariantLaws[F] {
  implicit override def F: Semigroupal[F]

  def semigroupalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, B, C)]] =
    (F.imap(F.product(fa, F.product(fb, fc)))
      { case (a, (b, c)) => (a, b, c) }
      { case (a, b, c) => (a, (b, c)) }) <-> F.imap(F.product(F.product(fa, fb), fc))
      { case ((a, b), c) => (a, b, c) }
      { case (a, b, c) => ((a, b), c) }

}
object SemigroupalLaws {
  def apply[F[_]](implicit ev: Semigroupal[F]): SemigroupalLaws[F] =
    new SemigroupalLaws[F] { def F: Semigroupal[F] = ev }
}
