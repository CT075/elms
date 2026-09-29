package elms.core

import elms.core.poly.Lift
import elms.core.tree.Note

trait Base extends Lift {
  protected type Exp

  def fun[A: Typable, B: Typable](name: Option[Name])(
      f: Rep[A] => Rep[B]
  ): Rep[A => B]
  def lam[A: Typable, B: Typable](f: Rep[A] => Rep[B]): Rep[A => B]

  def fun[A: Typable, B: Typable](f: Rep[A] => Rep[B]): Rep[A => B] = fun(None)(f)
  def fun[A: Typable, B: Typable](name: Name)(f: Rep[A] => Rep[B]): Rep[A => B] =
    fun(Some(name))(f)
  def fun[A: Typable, B: Typable](name: String)(f: Rep[A] => Rep[B]): Rep[A => B] =
    fun(Name.from(name))(f)

  def region[A](exp: => Rep[A]): Rep[A]

  def staticData[A: AsStaticData](data: A): Rep[A]

  def unsafeWrap[T](exp: Exp): Rep[T]
  def unsafeUnwrap[T](rep: Rep[T]): Exp
  def unsafeRegister(op: Op, children: Exp*): Exp

  def unsafeLift[A: Liftable](x: A): Exp = unsafeUnwrap(unit(x))

  def unsafeReflect[T](op: Op, children: Rep[Any]*): Rep[T] =
    unsafeWrap(unsafeRegister(op, children.map(unsafeUnwrap)*))

  // Neither of these gives back a `Rep`. An annotation is not a value: nothing
  // may name one, and reflecting a `unit(())` beside it would put a binding in
  // the residue that the source never asked for.
  def unsafeNote(
      parts: Seq[String],
      args: Seq[Rep[Any]],
      side: Note.Side,
      meta: Option[CommentMeta]
  ): Unit

  def unsafeContract(
      parts: Seq[String],
      args: Seq[Rep[Any]],
      side: Note.Side,
      meta: Option[CommentMeta]
  ): Unit

  def unsafeDeclare[T](name: String): Rep[T]

  def unsafeWithFresh[A, B](f: (Name, Rep[A]) => Rep[B]): Rep[B]
}
