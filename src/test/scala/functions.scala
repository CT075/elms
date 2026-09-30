package elms.test

import scala.language.implicitConversions

import elms.prelude._
import elms.prelude.given
import elms.helpers.DslOps

// Tests to ensure that implicit resolution is set up correctly.
// These should all typecheck and compile.
trait RepFunTypecheckSuite extends DslOps {
  def void(f: Rep[() => Int]) = f()
  def unary(f: Rep[Int => Int]) = f(1)
  def binary(f: Rep[(Int, String) => Int]) = f(1, "")
  def trinary(f: Rep[(Int, String, Boolean) => Int]) = f(1, "", false)
}

// `DslDriver` runs the eqsat builder, so a test that wants the simple one has
// to say so.
abstract class SimpleEvalDriver[A: Typable, B: Typable]
    extends elms.helpers.SnippetDriver[A, B] with DslOps with EvalScalaSnippet[A, B] {
  override val builder = elms.pipeline.simple.Builder()
}

// Two `fun` calls in one body, which nothing else in the suite does: every
// other "function" in these tests is a plain Scala method the front end inlines
// at staging time, so nothing ever reached `fill` a second time.
//
// Both builders, because only one of them had the bug and the pair of them
// agreeing is the property worth keeping.
@virtualize
class NestedFunTests extends org.scalatest.funsuite.AnyFunSuite {
  val inputs = Seq(0, 1, 7, -3, 100)

  // Five distinct answers, so a snippet that lost one of the two calls could
  // not pass by landing on the right number anyway.
  def check(eval: Int => Int, expected: Int => Int, what: String): Unit = {
    assert(inputs.map(expected).distinct.length == inputs.length)
    inputs.foreach { n => assert(eval(n) == expected(n), s"$what disagreed on $n") }
  }

  def expected(n: Int): Int = n * 2 + (n + 1) * 3

  test("two functions, simple builder") {
    object Snippet extends SimpleEvalDriver[Int, Int] {
      val prefix = "nested-fun"
      val name = "twoSimple"

      def twice(n: Rep[Int]): Rep[Int] = n * unit(2)
      def thrice(n: Rep[Int]): Rep[Int] = n * unit(3)

      def snippet(x: Rep[Int]): Rep[Int] = fun(twice)(x) + fun(thrice)(x + unit(1))
    }
    check(Snippet.eval, expected, "simple")
  }

  test("two functions, eqsat builder") {
    object Snippet extends DslDriver[Int, Int] with EvalScalaSnippet[Int, Int] {
      val prefix = "nested-fun"
      val name = "twoEqsat"

      def twice(n: Rep[Int]): Rep[Int] = n * unit(2)
      def thrice(n: Rep[Int]): Rep[Int] = n * unit(3)

      def snippet(x: Rep[Int]): Rep[Int] = fun(twice)(x) + fun(thrice)(x + unit(1))
    }
    check(Snippet.eval, expected, "eqsat")
  }

  // Three calls, so the block has to survive two more `fill`s after the first
  // statement went into it rather than just one.
  test("three functions, simple builder") {
    object Snippet extends SimpleEvalDriver[Int, Int] {
      val prefix = "nested-fun"
      val name = "threeSimple"

      def a(n: Rep[Int]): Rep[Int] = n * unit(2)
      def b(n: Rep[Int]): Rep[Int] = n * unit(3)
      def c(n: Rep[Int]): Rep[Int] = n * unit(5)

      def snippet(x: Rep[Int]): Rep[Int] =
        fun(a)(x) + fun(b)(x + unit(1)) + fun(c)(x + unit(2))
    }
    check(Snippet.eval, n => n * 2 + (n + 1) * 3 + (n + 2) * 5, "three")
  }
}
