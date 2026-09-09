package elms.pipeline.eqsat

import elms.core.Op
import elms.core.instances.given

import Pattern.{Var => PVar, Node => PNode}

// The identities the pipeline saturates with. Most of these used to live
// syntactically in `pipeline.Propagate`, where they could only fire on the exact
// shape the builder happened to emit; in the graph they fire on anything the rest
// of the set can reach.
//
// `Plus`, `Minus`, `Times`, `Negate` and the bitwise ops are Int-only in ELMS,
// so the arithmetic here is total and the constants below are unambiguous.
object Rules {
  private val x = PVar("x")
  private val y = PVar("y")

  private val zero = PNode(Op.Const(0), Vector())
  private val one = PNode(Op.Const(1), Vector())
  private val allOnes = PNode(Op.Const(-1), Vector())

  private def op(o: Op.Pure, args: Pattern*) = PNode(o, args.toVector)

  // No free associativity or commutativity, deliberately. Associativity's
  // right-hand side builds a sub-term that need not already exist, so its applier
  // manufactures e-classes; with commutativity feeding it every argument order
  // the graph grows about 2.7x per iteration and never stops. On `y + (x + -y)`
  // that is 412 nodes with backoff and 16927 without, against 6 for the rules
  // below, which reach the same answer.
  //
  // What the general rules were actually buying was cancellation and dragging
  // constants together for `constantFold`, so those are spelled out instead.
  // The cost is visible: every identity commutativity used to cover for free
  // needs its mirror written out, and cancellation only reaches the nesting
  // depths enumerated here.
  val default: Ruleset = Ruleset(
    Seq(
      // Identities, both orders, since nothing commutes for us any more.
      Rule.rewrite(op(Op.Plus, x, zero), x),
      Rule.rewrite(op(Op.Plus, zero, x), x),
      Rule.rewrite(op(Op.Minus, x, zero), x),
      Rule.rewrite(op(Op.Minus, zero, x), op(Op.Negate, x)),
      Rule.rewrite(op(Op.Minus, x, x), zero),
      Rule.rewrite(op(Op.Times, x, one), x),
      Rule.rewrite(op(Op.Times, one, x), x),
      Rule.rewrite(op(Op.Times, x, zero), zero),
      Rule.rewrite(op(Op.Times, zero, x), zero),
      Rule.rewrite(op(Op.Plus, x, op(Op.Negate, y)), op(Op.Minus, x, y)),

      // Cancellation. Free associativity and commutativity used to reach these by
      // exploring every re-bracketing; here they are, at depth one, by hand.
      Rule.rewrite(op(Op.Plus, x, op(Op.Negate, x)), zero),
      Rule.rewrite(op(Op.Plus, op(Op.Negate, x), x), zero),
      Rule.rewrite(op(Op.Plus, x, op(Op.Plus, y, op(Op.Negate, x))), y),
      Rule.rewrite(op(Op.Plus, x, op(Op.Plus, op(Op.Negate, x), y)), y),
      Rule.rewrite(op(Op.Plus, op(Op.Plus, x, y), op(Op.Negate, x)), y),
      Rule.rewrite(op(Op.Plus, op(Op.Plus, x, y), op(Op.Negate, y)), x),

      // Bitwise identities. Every rewrite here either drops a node or swaps one
      // for a smaller one, so nothing in this block can grow the graph the way
      // associativity did.
      Rule.rewrite(op(Op.BitAnd, x, x), x),
      Rule.rewrite(op(Op.BitOr, x, x), x),
      Rule.rewrite(op(Op.BitXor, x, x), zero),
      Rule.rewrite(op(Op.BitAnd, x, zero), zero),
      Rule.rewrite(op(Op.BitAnd, zero, x), zero),
      Rule.rewrite(op(Op.BitAnd, x, allOnes), x),
      Rule.rewrite(op(Op.BitAnd, allOnes, x), x),
      Rule.rewrite(op(Op.BitOr, x, zero), x),
      Rule.rewrite(op(Op.BitOr, zero, x), x),
      Rule.rewrite(op(Op.BitOr, x, allOnes), allOnes),
      Rule.rewrite(op(Op.BitOr, allOnes, x), allOnes),
      Rule.rewrite(op(Op.BitXor, x, zero), x),
      Rule.rewrite(op(Op.BitXor, zero, x), x),
      Rule.rewrite(op(Op.BitXor, x, allOnes), op(Op.BitNot, x)),
      Rule.rewrite(op(Op.BitXor, allOnes, x), op(Op.BitNot, x)),
      Rule.rewrite(op(Op.BitNot, op(Op.BitNot, x)), x),

      // Shifting by nothing, and shifting nothing. Neither wants the range guard
      // `ConstantAnalysis.inWidth` applies to folding: a zero count is in range
      // everywhere, and zero has no bits for a count to move.
      Rule.rewrite(op(Op.Shl, x, zero), x),
      Rule.rewrite(op(Op.Shr, x, zero), x),
      Rule.rewrite(op(Op.UShr, x, zero), x),
      Rule.rewrite(op(Op.Shl, zero, x), zero),
      Rule.rewrite(op(Op.Shr, zero, x), zero),
      Rule.rewrite(op(Op.UShr, zero, x), zero)
    ),
    Ruleset.collectConstants(Op.Plus) ++
      Ruleset.collectConstants(Op.Times) ++
      Ruleset.collectConstants(Op.BitAnd) ++
      Ruleset.collectConstants(Op.BitOr) ++
      Ruleset.collectConstants(Op.BitXor)
  )
}
