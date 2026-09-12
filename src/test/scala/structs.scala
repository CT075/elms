package elms.test

import scala.language.implicitConversions

import elms.prelude.{_, given}
import elms.helpers.OptimizingSnippetDriver
import elms.pipeline.eqsat.Ruleset
import elms.helpers.DslOps
import elms.core.StructManifest
import elms.codegen.CCodegen

@virtualize
class StructTests extends SnapshotFunSuite {
  val under = "cstruct/"

  override def check(
      label: String,
      actual: String,
      ext: String = "c",
      accept: Boolean = false
  ) = super.check(label, actual, ext, accept)

  abstract class DslDriverC[A: Typable, B: Typable]
      extends OptimizingSnippetDriver[A, B](Ruleset(Seq())) with DslOps {
    override val codegen = CCodegen()
  }

  case class Foo(x: Int, y: String) derives StructManifest
  case class Bar(z: Int) derives StructManifest
  case class Nested(b: Bar, n: Int) derives StructManifest

  // Six fields, so `members` is past the size where a plain `Map` would keep
  // them in declaration order.
  case class Wide(a: Int, b: Int, c: Int, d: Int, e: Int, f: String)
      derives StructManifest

  test("get") {
    object Snippet extends DslDriverC[Foo, Int] with DslOps {
      def snippet(s: Rep[Foo]): Rep[Int] = { s.get("x").asInstanceOf[Rep[Int]] }
    }
    check("get", Snippet.code)
  }

  test("set") {
    object Snippet extends DslDriverC[Foo, Unit] with DslOps {
      def snippet(s: Rep[Foo]): Rep[Unit] = { s.set("x", 6) }
    }
    check("set", Snippet.code)
  }

  test("nested struct") {
    object Snippet extends DslDriverC[Nested, Int] with DslOps {
      def snippet(s: Rep[Nested]): Rep[Int] = { s.get("n").asInstanceOf[Rep[Int]] }
    }
    check("nested", Snippet.code)
  }

  test("fields keep their declaration order") {
    object Snippet extends DslDriverC[Wide, Int] with DslOps {
      def snippet(s: Rep[Wide]): Rep[Int] = { s.get("c").asInstanceOf[Rep[Int]] }
    }
    check("wide", Snippet.code)
  }

}
