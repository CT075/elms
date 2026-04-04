// Contains the operations supported by LMS.

package lms.core

enum Op(effectful: Boolean = true) derives CanEqual {
  case Const[T](val v: T)

  case App extends Op(effectful = true)

  case Plus
  case Minus
  case Times

  case Equals
  case Lt
  case Gt
  case Le
  case Ge

  case And
  case Or

  case Range
  case RangeForEach extends Op(effectful = true)
  case RangeStart
  case RangeEnd

  case IfThenElse
  case While

  case ArrayNew(typ: Type) extends Op(effectful = true)
  case ArrayGet extends Op(effectful = true)
  case ArraySet extends Op(effectful = true)
  case ArrayLength
}
