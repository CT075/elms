package elms.pipeline.eqsat

import scala.collection.mutable

import foresight.eqsat.MixedTree
import foresight.eqsat.readonly
import foresight.eqsat.rewriting
import foresight.eqsat.rewriting.patterns.{Pattern as FPattern, PatternMatch}

import elms.core.Op

enum Pattern {
  case Var(name: String)
  case Node(op: Op.Pure, children: Vector[Pattern])
}

object Pattern {
  def render(p: Pattern): String = p match {
    case Var(name)               => s"?$name"
    case Node(op, Vector())      => op.toString
    case Node(op, children)      => s"$op(${children.map(render).mkString(", ")})"
  }
}

abstract class Rule private (
    val lhs: Pattern,
    val rhs: Pattern,
    val symmetric: Boolean
) {
  private[eqsat] def compile: Seq[Ruleset.Compiled] =
    if symmetric then Seq(Rule.directed(lhs, rhs), Rule.directed(rhs, lhs))
    else Seq(Rule.directed(lhs, rhs))
}

object Rule {
  private case class Rewrite(lhsp: Pattern, rhsp: Pattern)
      extends Rule(lhsp, rhsp, false)
  private case class Equivalence(lhsp: Pattern, rhsp: Pattern)
      extends Rule(lhsp, rhsp, true)

  def rewrite(lhs: Pattern, rhs: Pattern): Rule = Rewrite(lhs, rhs)
  def equivalence(lhs: Pattern, rhs: Pattern): Rule = Equivalence(lhs, rhs)

  // ELMS names its pattern variables with strings and foresight with identity, so
  // one `FPattern.Var` per name per rule. The left side is compiled first: every
  // name the right side mentions has to have been bound there.
  private def directed(lhs: Pattern, rhs: Pattern): Ruleset.Compiled = {
    val vars = mutable.Map.empty[String, FPattern.Var]

    def go(p: Pattern): MixedTree[ElmsNode, FPattern.Var] = p match {
      case Pattern.Var(name) => MixedTree
          .Atom(vars.getOrElseUpdate(name, FPattern.Var.fresh()))
      case Pattern.Node(op, children) =>
        MixedTree.unslotted(ElmsNode.Pure(op), children.map(go))
    }

    val searcher = go(lhs)
    val bound = vars.keySet.toSet
    val applier = go(rhs)

    require(
      vars.keySet.subsetOf(bound),
      s"unbound pattern variables in ${Pattern.render(rhs)}: " +
        (vars.keySet -- bound).mkString(", ")
    )

    rewriting.Rule(
      s"${Pattern.render(lhs)} => ${Pattern.render(rhs)}",
      searcher.toSearcher[Ruleset.Graph],
      applier.toApplier[Ruleset.Graph]
    )
  }
}

object Ruleset {
  // Rules are written against the plainest graph type that supports them, and
  // `Rewrite` is contravariant in it, so they still apply to the metadata-carrying
  // mutable graph `EGraph` actually holds.
  type Graph = readonly.EGraph[ElmsNode]
  type Compiled = rewriting.Rule[ElmsNode, PatternMatch[ElmsNode], Graph]
}

class Ruleset(ruleDecls: Seq[Rule]) {
  // Names encode the rewrite, so this is the old dedup by `Set[Expansion]` and it
  // also satisfies foresight's requirement that rule names be unique.
  val compiled: Seq[Ruleset.Compiled] = ruleDecls.flatMap(_.compile).distinctBy(_.name)
}
