package elms.core.tree.typed

import elms.core.given
import elms.core.{Type, Typable, Primitive, Op, Name, StructRepr, StructManifest}
import elms.core.tree.untyped

sealed trait Term[T: Typable]

case class V[T: Typable](name: Name) extends Term[T]

case class Function[A: Typable, B: Typable](arg: Name, body: Term[B])
    extends Term[A => B]

case class Const[T: Primitive](x: T) extends Term[T]
case class Let[A: Typable, B: Typable](x: Name, e1: Term[A], e2: Term[B])
    extends Term[B]

case class IfThenElse[A: Typable](guard: Term[Boolean], thent: Term[A], elset: Term[A])
  extends Term[A]
case class While(guard: Term[Boolean], body: Term[Unit])

case class App[A: Typable, B: Typable](f: Term[A => B], x: Term[A]) extends Term[B]

case class Negate(t: Term[Int]) extends Term[Int]
case class Add(x: Term[Int], y: Term[Int]) extends Term[Int]
case class Sub(x: Term[Int], y: Term[Int]) extends Term[Int]
case class Mul(x: Term[Int], y: Term[Int]) extends Term[Int]

case class Equals[A: Typable](x: Term[A], y: Term[A]) extends Term[Boolean]

case class Lt(x: Term[Int], y: Term[Int]) extends Term[Boolean]
case class Gt(x: Term[Int], y: Term[Int]) extends Term[Boolean]
case class Le(x: Term[Int], y: Term[Int]) extends Term[Boolean]
case class Ge(x: Term[Int], y: Term[Int]) extends Term[Boolean]

case class Not(x: Term[Boolean]) extends Term[Boolean]
case class And(x: Term[Boolean], y: Term[Boolean]) extends Term[Boolean]
case class Or(x: Term[Boolean], y: Term[Boolean]) extends Term[Boolean]

case class ArrayNew[A: Typable](len: Term[Int]) extends Term[Array[A]]
case class ArrayGet[A: Typable](xs: Term[Array[A]], i: Term[Int]) extends Term[A]
case class ArraySet[A: Typable](xs: Term[Array[A]], i: Term[Int], x: Term[A]) extends Term[Unit]
