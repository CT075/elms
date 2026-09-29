// Contains the operations supported by LMS.

package elms.core

sealed trait Op derives CanEqual

object Op {
  sealed abstract class Pure extends Op

  // An effect, split by what it does to memory.
  //
  // A `Read` observes the store: it has to stay in order relative to a write,
  // but nothing forces it to be emitted if no one wants its value, and two of
  // them with no write between are the same value.
  //
  // A `Write` changes the store, or does something the compiler cannot see
  // through such as I/O or a call. Always emitted, always in order.
  sealed abstract class Effectful extends Op
  sealed abstract class Read extends Effectful
  sealed abstract class Write extends Effectful

  sealed abstract class Control extends Op

  case class Const[T](val v: T)(using val prim: Primitive[T]) extends Pure

  case class VarNew(val typ: Type) extends Write
  case object VarGet extends Read
  case object VarSet extends Write

  case object App extends Write

  case object Negate extends Pure
  case object Plus extends Pure
  case object Minus extends Pure
  case object Times extends Pure

  case object Equals extends Pure
  case object Lt extends Pure
  case object Gt extends Pure
  case object Le extends Pure
  case object Ge extends Pure

  case object Not extends Pure
  case object And extends Control
  case object Or extends Control

  // `And` and `Or` above take regions and compile to an `if`, which is what
  // makes them `Control`. These take values, so they are `Pure` and the rewrite
  // rules can see them. `Xor` needs no qualifier: there is no lazy form of it.
  case object StrictAnd extends Pure
  case object StrictOr extends Pure
  case object Xor extends Pure

  case object BitAnd extends Pure
  case object BitOr extends Pure
  case object BitXor extends Pure
  case object BitNot extends Pure
  case object Shl extends Pure
  // `Shr` keeps the sign bit and `UShr` shifts in zeroes, exactly Scala's `>>`
  // and `>>>`. C has no operator for the latter, so `CCodegen` routes it
  // through `unsigned int`.
  case object Shr extends Pure
  case object UShr extends Pure

  case object Print extends Write
  case object Println extends Write

  case object StringLength extends Pure
  case object StringTake extends Pure
  case object StringDrop extends Pure
  case object StringStartsWith extends Pure
  case object StringCharAt extends Pure
  case object StringEndsWith extends Pure
  case object StringSubstring extends Pure

  case object Range extends Pure
  case class RangeForEach(x: Name) extends Control
  case object RangeStart extends Pure
  case object RangeEnd extends Pure

  case object IfThenElse extends Control
  case object While extends Control

  case class ArrayNew(val typ: Type) extends Write
  // The element type is named rather than left to a context bound, so a backend
  // can read it off the op the way it reads `ArrayNew`'s.
  case class ArrayInit[T](init: Seq[T])(using val elem: Typable[T]) extends Write {
    def elemTy: Type = elem.identity
  }
  case object ArrayGet extends Read
  case object ArraySet extends Write
  case object ArrayLength extends Pure

  case class StructGet(val repr: StructRepr, val field: String) extends Read
  case class StructSet(val field: String) extends Write

  case class Custom(val name: String, val ty: Type) extends Write
}
