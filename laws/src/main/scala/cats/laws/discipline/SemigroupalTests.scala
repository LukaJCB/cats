package cats
package laws
package discipline

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}

trait SemigroupalTests[F[_]] extends InvariantTests[F] {
  def laws: SemigroupalLaws[F]

  def semigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                            arbFA: Arbitrary[F[A]],
                                                            arbFB: Arbitrary[F[B]],
                                                            arbFC: Arbitrary[F[C]],
                                                            CogenA: Cogen[A],
                                                            CogenB: Cogen[B],
                                                            CogenC: Cogen[C],
                                                            EqFA: Eq[F[A]],
                                                            EqFB: Eq[F[B]],
                                                            EqFC: Eq[F[C]],
                                                            EqFABC: Eq[F[(A, B, C)]]): RuleSet = new RuleSet {
    val name = "semigroupal"
    val parents = Seq(invariant[A, B, C])
    val bases = Nil
    val props = Seq("semigroupal associativity" ->
      forAll((fa: F[A], fb: F[B], fc: F[C]) => laws.semigroupalAssociativity(fa, fb, fc)))

  }
}

object SemigroupalTests {
  def apply[F[_]: Semigroupal]: SemigroupalTests[F] =
    new SemigroupalTests[F] { def laws: SemigroupalLaws[F] = SemigroupalLaws[F] }
}
