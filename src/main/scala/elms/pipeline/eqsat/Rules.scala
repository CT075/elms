package elms.pipeline.eqsat

import elms.core.Op
import elms.core.instances.given

import Pattern.{Var => PVar, Node => PNode}

// The identities the pipeline saturates with. Most of these used to live
// syntactically in `pipeline.Propagate`, where they could only fire on the exact
// shape the builder happened to emit; in the graph they fire on anything the rest
// of the set can reach.
//
// `Plus`, `Minus`, `Times` and `Negate` are Int-only in ELMS, so the arithmetic
// here is total and the zero and one below are unambiguous.
object Rules {
  private val x = PVar("x")
  private val y = PVar("y")
  private val z = PVar("z")

  private val zero = PNode(Op.Const(0), Vector())
  private val one = PNode(Op.Const(1), Vector())

  private def op(o: Op.Pure, args: Pattern*) = PNode(o, args.toVector)

  val default: Seq[Rule] = Seq(
    Rule.equivalence(op(Op.Plus, x, y), op(Op.Plus, y, x)),
    Rule.equivalence(op(Op.Times, x, y), op(Op.Times, y, x)),
    Rule.equivalence(
      op(Op.Plus, x, op(Op.Plus, y, z)),
      op(Op.Plus, op(Op.Plus, x, y), z)
    ),
    Rule.equivalence(
      op(Op.Times, x, op(Op.Times, y, z)),
      op(Op.Times, op(Op.Times, x, y), z)
    ),
    // One way only. Backwards it puts `x - 1` and `x + -1` in one class at the
    // same node count, and which of the two comes out is then down to the
    // tie-break rather than to anything meaningful.
    Rule.rewrite(op(Op.Plus, x, op(Op.Negate, y)), op(Op.Minus, x, y)),
    Rule.rewrite(op(Op.Plus, x, zero), x),
    Rule.rewrite(op(Op.Minus, x, zero), x),
    Rule.rewrite(op(Op.Minus, zero, x), op(Op.Negate, x)),
    Rule.rewrite(op(Op.Minus, x, x), zero),
    Rule.rewrite(op(Op.Times, x, one), x),
    Rule.rewrite(op(Op.Times, x, zero), zero)
  )
}
