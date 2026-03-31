package lms.ir.eqsat

import scala.collection.mutable

import lms.core
import lms.codegen.ast.*

class EGraph {
  // CR cwong: We should instead move `V` into `core.Op`.
  enum Op derives CanEqual {
    case C(inner: core.Op)
    case V(name: String)
  }
  import Op._

  case class EC(id: Int) derives CanEqual
  case class Node(op: Op, children: Vector[EClass])

  opaque type EClass = EC
  opaque type ENode = Node

  private class UnionFind {
    val parents: mutable.ArrayBuffer[EClass] = mutable.ArrayBuffer()

    def fresh: EClass = {
      val result = EC(parents.length)
      parents += result
      result
    }

    def find(ec: EClass): EClass = {
      val parent = parents(ec.id)
      if parent.id == ec.id then ec
      else {
        val result = find(parent)
        parents(ec.id) = result
        result
      }
    }

    def union(aIn: EClass, bIn: EClass): EClass = {
      val a = find(aIn)
      val b = find(bIn)
      if a == b then a
      else {
        parents(a.id) = b
        b
      }
    }

    def equals(a: EClass, b: EClass): Boolean = find(a) == find(b)
  }

  private val uf: UnionFind = new UnionFind

  private def canonicalize(node: ENode): ENode =
    Node(node.op, node.children.map(uf.find))

  private val nodes: mutable.Map[ENode, EClass] = mutable.Map()
  private val classes: mutable.Map[EClass, mutable.Set[ENode]] = mutable.Map()

  private def ensureClass(ec: EClass): mutable.Set[ENode] = classes
    .getOrElseUpdate(uf.find(ec), mutable.Set())

  def addNode(nodeIn: ENode): EClass = {
    val node = canonicalize(nodeIn)
    nodes.get(node) match {
      case Some(cls) => uf.find(cls)
      case None      => {
        val result = uf.fresh
        nodes(node) = result
        ensureClass(result) += node
        result
      }
    }
  }

  def add(op: Op, children: Seq[EClass]): EClass = addNode(Node(op, children.toVector))

  def merge(a: EClass, b: EClass): Boolean = {
    if uf.equals(a, b) then false
    else {
      val root = uf.union(a, b)
      val other = if a == root then uf.find(b) else uf.find(a)
      val rootNodes = ensureClass(root)
      classes.get(other).foreach(rootNodes ++= _)
      classes.remove(other)

      true
    }
  }

  def nodesInClass(cls: EClass): Set[ENode] = classes(cls).toSet

  def rebuild(): Unit = {
    var changed = true
    while changed do {
      changed = false

      val oldClasses = classes.toList
      nodes.clear()
      classes.clear()

      oldClasses.foreach { case (cls0, nodes0) =>
        val cls = uf.find(cls0)
        nodes0.foreach { node0 =>
          val node = canonicalize(node0)
          nodes.get(node) match {
            case Some(existing) => changed = merge(cls, existing)

            case None => {
              val root = uf.find(cls)
              nodes(node) = root
              ensureClass(root) += node
            }
          }
        }
      }
    }
  }

  type Subst = Map[String, EClass]

  private def matchPattern(
      pat: Pattern,
      cls: EClass,
      subst: Subst = Map.empty
  ): Seq[Subst] = pat match {
    case PVar(name) => subst.get(name) match {
        case None        => Seq(subst + (name -> uf.find(cls)))
        case Some(bound) => if uf.equals(bound, cls) then Seq(subst) else Seq.empty
      }

    case PNode(op, pats) => for {
        node <- nodesInClass(cls).toSeq if node.op == C(op)
        subst2 <- matchChildren(pats, node.children, subst)
      } yield subst2
  }

  private def matchChildren(
      pats: Vector[Pattern],
      children: Vector[EClass],
      subst: Subst
  ): Seq[Subst] =
    if pats.length != children.length then Seq.empty
    else {
      pats.zip(children).foldLeft(Seq(subst)) { case (acc, (p, c)) =>
        for {
          s <- acc
          s2 <- matchPattern(p, c, s)
        } yield s2
      }
    }

  private def build(pat: Pattern, subst: Subst): EClass = pat match {
    case PVar(name) => subst(name)
    case PNode(op, children) => add(C(op), children.map(build(_, subst)))
  }

  def applyRule(rule: Rule): Unit = {
    val snapshot = classes.keys.toList
    snapshot.foreach { cls =>
      val matches = matchPattern(rule.lhs, cls)
      matches.foreach { subst =>
        val rhsCls = build(rule.rhs, subst)
        merge(cls, rhsCls)
      }
    }
  }

  def applyRules(rules: Seq[Rule]): Unit = {
    rules.foreach(applyRule)
    rebuild()
  }

  // CR-soon cwong: At the moment, we hardcode AST size as the metric to optimize
  // for. We probably want to generalize this.
  def extract(cls: EClass): Program = {
    rebuild()
  }
}
