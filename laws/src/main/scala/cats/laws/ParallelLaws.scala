package cats
package laws


/**
  * Laws that must be obeyed by any `cats.Parallel`.
  */
trait ParallelLaws[M[_], F[_]] {
  def monadM: Monad[M]
  def P: Parallel[M, F]

  def parallelRoundTrip[A](ma: M[A]): IsEq[M[A]] =
    P.sequential(monadM)(P.parallel(monadM)(ma)) <-> ma

  def sequentialRoundTrip[A](fa: F[A]): IsEq[F[A]] =
    P.parallel(monadM)(P.sequential(monadM)(fa)) <-> fa
}

object ParallelLaws {
  def apply[M[_]: Monad, F[_]](implicit ev: Parallel[M, F]): ParallelLaws[M, F] =
    new ParallelLaws[M, F] { def P: Parallel[M, F] = ev;  def monadM: Monad[M] = Monad[M] }
}
