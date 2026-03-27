package lms.test

import lms.prelude._
import lms.helpers.DslOps

// Tests to ensure that implicit resolution is set up correctly.
trait RepFunTypecheckSuite extends DslOps {
  def foo(f: Rep[Int => Int]) = f(1)

  // We should be able to call this as `f((unit(1), unit("")))`, if not `f(1, "")`.
  // However, implicit resolution doesn't seem to be able to determine that
  // `(Rep[Int], Rep[String])` can be `UnwrapTupleRep`ed without help. The type
  // annotations on this `call` invocation are also mandatory.
  def bar(f: Rep[(Int, String) => Int]) =
    //f((unit(1), unit("")))
    call[(Int, String) => Int, (Int, String), Int](f, (unit(1), unit("")))
}
