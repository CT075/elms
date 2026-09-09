package elms.test

import scala.collection.mutable

import org.scalatest.funsuite.AnyFunSuite

import elms.prelude.{_, given}
import elms.core.Name
import elms.core.Op.*
import elms.core.tree.*
import elms.pipeline.eqsat.*
import elms.pipeline.eqsat.EGraph.EClass

import Pattern.{Var => PVar, Node => PNode}

class EqsatSuite extends AnyFunSuite {
  private def emptyGraph = new EGraph(Ruleset(Seq()))

  private def v(name: String): Term = V(Name.from(name))

  private def extracted(g: EGraph, cls: EClass): Term = g.extract(cls)
    .getOrElse(fail("nothing extracted"))

  // `y + (x + -y)`, the term the playground has always used as its worked
  // example, together with the rules that take it down to `x`.
  private val addcomm = Rule.equivalence(
    PNode(Plus, Vector(PVar("x"), PVar("y"))),
    PNode(Plus, Vector(PVar("y"), PVar("x")))
  )
  private val addassoc = Rule.equivalence(
    PNode(Plus, Vector(PVar("x"), PNode(Plus, Vector(PVar("y"), PVar("z"))))),
    PNode(Plus, Vector(PNode(Plus, Vector(PVar("x"), PVar("y"))), PVar("z")))
  )
  private val subnegate = Rule.equivalence(
    PNode(Plus, Vector(PVar("x"), PNode(Negate, Vector(PVar("y"))))),
    PNode(Minus, Vector(PVar("x"), PVar("y")))
  )
  private val subself = Rule
    .rewrite(PNode(Minus, Vector(PVar("x"), PVar("x"))), PNode(Const(0), Vector()))
  private val addzero = Rule
    .rewrite(PNode(Plus, Vector(PVar("x"), PNode(Const(0), Vector()))), PVar("x"))

  private val arith = Ruleset(Seq(addcomm, addassoc, subnegate, subself, addzero))

  test("structurally equal nodes hashcons to one class") {
    val g = emptyGraph
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")

    assert(g.addNamedVar("x") == x)
    assert(x != y)
    assert(g.addNode(Plus, Seq(x, y)) == g.addNode(Plus, Seq(x, y)))
    assert(g.addNode(Plus, Seq(x, y)) != g.addNode(Plus, Seq(y, x)))
    assert(g.addNode(Plus, Seq(x, y)) != g.addNode(Minus, Seq(x, y)))
  }

  test("rebuild closes congruence through nested nodes") {
    val g = emptyGraph
    val a = g.addNamedVar("a")
    val b = g.addNamedVar("b")

    def wrap(depth: Int, cls: EClass): EClass =
      if depth == 0 then cls else wrap(depth - 1, g.addNode(Negate, Seq(cls)))

    val nesteda = wrap(3, a)
    val nestedb = wrap(3, b)
    assert(!g.sameClass(nesteda, nestedb))

    g.union(a, b)
    g.rebuild()
    assert(g.sameClass(nesteda, nestedb))
  }

  test("ematch binds each pattern variable to the class it matched") {
    val g = emptyGraph
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")
    val sum = g.addNode(Plus, Seq(x, y))

    val substs = g.ematch(PNode(Plus, Vector(PVar("a"), PVar("b"))), sum)
    assert(substs.length == 1)
    assert(substs.head("a") == x)
    assert(substs.head("b") == y)

    assert(g.ematch(PNode(Minus, Vector(PVar("a"), PVar("b"))), sum).isEmpty)
  }

  test("a repeated pattern variable only matches equal classes") {
    val g = emptyGraph
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")
    val pat = PNode(Plus, Vector(PVar("a"), PVar("a")))

    assert(g.ematch(pat, g.addNode(Plus, Seq(x, y))).isEmpty)
    assert(g.ematch(pat, g.addNode(Plus, Seq(x, x))).length == 1)
  }

  test("extraction picks the smallest term in a class") {
    val g = emptyGraph
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")

    val small = g.addNode(Plus, Seq(x, y))
    val big = g
      .addNode(Plus, Seq(x, g.addNode(Plus, Seq(y, g.addNode(Negate, Seq(x))))))

    g.union(big, small)
    assert(extracted(g, big) == E(Plus, Seq(v("x"), v("y"))))
  }

  test("saturation rewrites y + (x + -y) down to x") {
    val g = new EGraph(arith)
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")
    val root = g
      .addNode(Plus, Seq(y, g.addNode(Plus, Seq(x, g.addNode(Negate, Seq(y))))))

    g.saturate()
    assert(extracted(g, root) == v("x"))
  }

  test("saturation reaches a fixpoint under rules that could loop") {
    val g = new EGraph(Ruleset(Seq(addcomm, addassoc)))
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")
    val z = g.addNamedVar("z")
    val root = g.addNode(Plus, Seq(x, g.addNode(Plus, Seq(y, z))))

    g.saturate()

    // Commutativity and associativity are both equivalences, so every
    // re-bracketing of `x + y + z` has to land in the root's class.
    val reassociated = g.addNode(Plus, Seq(g.addNode(Plus, Seq(z, y)), x))
    assert(g.sameClass(root, reassociated))
  }

  test("a rule that only merges classes is not reported as zero-yield") {
    val recorded = mutable.ArrayBuffer.empty[(Int, Int)]
    val spy = new Scheduler {
      def shouldRun(rule: Expansion, iteration: Int): Boolean = true
      def recordResult(
          rule: Expansion,
          iteration: Int,
          matches: Int,
          newNodes: Int,
          newUnions: Int
      ): Unit = recorded += ((newNodes, newUnions))
    }

    // Both orderings are already in the graph, so commutativity has no node to
    // add and the merge is the entire yield of the rule.
    val g = new EGraph(Ruleset(Seq(addcomm)), scheduler = spy)
    val x = g.addNamedVar("x")
    val y = g.addNamedVar("y")
    g.addNode(Plus, Seq(x, y))
    g.addNode(Plus, Seq(y, x))

    g.applyRules(0, false)
    assert(recorded.exists { (newNodes, newUnions) => newNodes == 0 && newUnions > 0 })
  }
}
