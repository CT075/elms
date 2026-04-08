package lms.ir.opt

import lms.core.Op

enum Pattern {
  case Var(name: String)
  case Node(op: Op, children: Vector[Pattern])
}

case class Rewrite(lhs: Pattern, rhs: Pattern)
//case class Equivalence(pats: Pattern*)
case class Equivalence(lhs: Pattern, rhs: Pattern)
