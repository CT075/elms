package lms.ir.eqsat

import lms.core.Op

sealed trait Pattern
case class PVar(name: String) extends Pattern
case class PNode(op: Op, children: Vector[Pattern]) extends Pattern

case class Rule(lhs: Pattern, rhs: Pattern)
