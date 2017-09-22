package cats.data

import cats.{Alternative, Eq}
import cats.instances.stream._

class ZipStream[A](val value: Stream[A]) extends AnyVal

object ZipStream {

  def apply[A](value: Stream[A]): ZipStream[A] = new ZipStream(value)

  implicit val catsDataAlternativeForZipStream: Alternative[ZipStream] = new Alternative[ZipStream] {
    def pure[A](x: A): ZipStream[A] = new ZipStream(Stream(x))
    def ap[A, B](ff: ZipStream[A => B])(fa: ZipStream[A]): ZipStream[B] =
      ZipStream((ff.value, fa.value).zipped.map(_ apply _))

    override def product[A, B](fa: ZipStream[A], fb: ZipStream[B]): ZipStream[(A, B)] =
      ZipStream(fa.value.zip(fb.value))

    def empty[A]: ZipStream[A] = ZipStream(Stream.empty[A])

    def combineK[A](x: ZipStream[A], y: ZipStream[A]): ZipStream[A] =
      ZipStream(Alternative[Stream].combineK(x.value, y.value))
  }

  implicit def catsDataEqForZipStream[A: Eq]: Eq[ZipStream[A]] = Eq.by(_.value)
}
