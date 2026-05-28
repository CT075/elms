package elms.core

import scala.deriving.Mirror
import scala.compiletime.{erasedValue, constValue, summonInline, error}

import elms.core.macros.Manifest

type Field[
  Name <: String,
  Labels <: Tuple,
  Elems <: Tuple
] <: Any = (Labels, Elems) match
  case (Name *: labelsTail, elem *: elemsTail) => elem
  case (label *: labelsTail, elem *: elemsTail) =>
    Field[Name, labelsTail, elemsTail]

inline def containsLabel[Name <: String, Labels <: Tuple]: Boolean =
  inline erasedValue[Labels] match
    case _: EmptyTuple => false
    case _: (Name *: tail) => true
    case _: (head *: tail) => containsLabel[Name, tail]

case class FieldWitness[A](typ: Typable[A])

trait StructManifest[S] {
  val name: String
  val fields: Map[String, Type]
}

object StructManifest {
  @annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline given derived[S](using Mirror.ProductOf[S]): StructManifest[S] =
    ${ Manifest.derivedImpl[S] }
}
