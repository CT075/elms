package lms.core

import scala.util.TupledFunction
import annotation.implicitNotFound

import lms.runtime.Log
import lms.core.Op._

trait PrimitiveOps extends Base {
  def __ifThenElse[T](c: Rep[Boolean], t: => Rep[T], e: => Rep[T]): Rep[T] =
    unsafeReflect(IfThenElse, c, region(t), region(e))

  given __virtualizedBoolConvInternal: Conversion[Rep[Boolean], Boolean] with
    def apply(x: Rep[Boolean]) = {
      throw new RuntimeException(
        "attempted to call __virtualizedBoolConvInternal (did you forget to virtualize?)"
      );
    }

  extension [T](using CanEqual[T, T])(lhs: Rep[T])
    def ===(rhs: Rep[T]): Rep[Boolean] = unsafeReflect(Equals, lhs, rhs)

  // At the moment, it's very difficult to use `==` for Rep operations, due to
  // the `.equals` always returning `Bool`. Previously, LMS would rewrite the
  // expression `a == b` into `a.__equals(b)` and let method overloading figure
  // out the rest. Unfortunately, TASTy makes this much more complicated.
  //
  // Because Scala 3 macro expansion happens post-typechecking, `a == b` is
  // always inferred to have type `Boolean`. By itself, this isn't a blocker;
  // we can rewrite a guard of the form `a == b` at the same time we rewrite an
  // if-expression.
  //
  // The problem comes when `a == b` is used as a subexpression or stored into
  // a variable. In that case, it is actually incorrect to locally rewrite
  // `a == b` into `a.__equals(b)`, because the two have different types. This,
  // too, is not insurmountable; we could rewrite something like `b1 && b2` to
  // use the `Rep` version of `&&`. However, that would necessarily always be
  // an ad-hoc process, ultimately turning into a game of macro error whack-a-mole
  // (both for symbolic operators and hardcoded named functions).
  //
  // Worse, because these errors happen at macro expansion time, they're
  // unlikely to be a good developer UX. It's much simpler to ban the use of
  // `==` on `Rep`s entirely and mint a dedicated operator instead.
  //
  // All that said, it's certainly possible that someone better versed in
  // how exactly TASTy macros interact with type inference could make this work.
  @implicitNotFound("`Rep`s should not be compared using `==`, use `===` instead.")
  sealed trait NoRepEquals[T]

  given [T: NoRepEquals](using CanEqual[T, T]): CanEqual[Rep[T], Rep[T]] =
    CanEqual.derived
  given [T: NoRepEquals](using CanEqual[T, T]): CanEqual[T, Rep[T]] = CanEqual.derived
  given [T: NoRepEquals](using CanEqual[T, T]): CanEqual[Rep[T], T] = CanEqual.derived

  extension [B](f: Rep[() => B]) def apply(): Rep[B] = unsafeReflect(App, f)

  extension [A, B](f: Rep[A => B])
    def apply(arg: Rep[A]): Rep[B] = unsafeReflect(App, f, arg)

  // !!! This *almost* works. While we can generalize over function arities
  // properly, Scala's inference search can't quite actually get the right types
  // for `call` without help (see also `typechecking` in tests).

  // The below is Some Magic that is intended to allow lifted functions of type
  // `Rep[(A, B) => C]` (for all tuple types) to be called with no special
  // syntax.

  // The trait `UnwrapTupleReps` defines a function `unwrapAll` that turns a
  // tuple of type `(Rep[A], Rep[B])` (etc) into a `List[Exp]`.
  trait UnwrapTupleReps[Args <: Tuple] {
    def unwrapAll(args: Tuple.Map[Args, Rep]): List[Exp]
  }

  // Next, we define the type-level recursive function over tuple types. An
  // empty tuple produces an empty list...
  given UnwrapTupleReps[EmptyTuple] with
    def unwrapAll(args: EmptyTuple): List[Exp] = Nil

  // And the tuple `Rep[H] *: rest` unwraps the head, then recursively unwraps
  // the tail.
  given [H, T <: Tuple](using tailUnwrapper: UnwrapTupleReps[T]): UnwrapTupleReps[
    H *: T
  ] with
    def unwrapAll(args: Rep[H] *: Tuple.Map[T, Rep]): List[Exp] =
      unsafeUnwrap(args.head) :: tailUnwrapper.unwrapAll(args.tail)

  def call[F, Args <: Tuple, B](f: Rep[F], args: Tuple.Map[Args, Rep])(using
      TupledFunction[F, Args => B],
      UnwrapTupleReps[Args]
  ): Rep[B] = unsafeWrap(
    unsafeRegister(
      App,
      (unsafeUnwrap(f) :: summon[UnwrapTupleReps[Args]].unwrapAll(args))*
    )
  )

  extension [F, Args <: Tuple, B](using
      TupledFunction[F, Args => B],
      UnwrapTupleReps[Args]
  )(f: Rep[F]) def apply(args: Tuple.Map[Args, Rep]): Rep[B] = call(f, args)
}
