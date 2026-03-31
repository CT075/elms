package lms.util

object Plumbing {
  def mapLeft[A1, A2, B](f: A1 => A2)(x: A1, y: B): (A2, B) = (f(x), y)
  def mapRight[A, B1, B2](f: B1 => B2)(x: A, y: B1): (A, B2) = (x, f(y))

  def replaceLeft[A1, A2, B](xy: (A1, B), x2: A2): (A2, B) = {
    val (_, y) = xy
    (x2, y)
  }

  def replaceRight[A, B1, B2](xy: (A, B1), y2: B2): (A, B2) = {
    val (x, _) = xy
    (x, y2)
  }

  extension [A](it: Iterable[A])
    def filterMap[B](f: A => Option[B]): Iterable[B] = it.collect(Function.unlift(f))
}
