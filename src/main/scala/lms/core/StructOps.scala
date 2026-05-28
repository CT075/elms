package elms.core

import scala.deriving.Mirror
import scala.compiletime.{constValue, error}

import elms.runtime.Log

// These are low-level operations that simply reflect the operation into the
// IR. If you want more strongly-typed getters and setters, please use a wrapper
// with `asInstanceOf`.

trait StructOps extends Base {
  transparent inline def structGet[S, Name <: String & Singleton](
      receiver: Rep[S]
  )(using
      manifest: StructManifest[S],
      m: Mirror.ProductOf[S]
  ): Rep[Field[Name, m.MirroredElemLabels, m.MirroredElemTypes]] =
    inline if containsLabel[Name, m.MirroredElemLabels] then
      unsafeReflect(Op.StructGet(manifest, constValue[Name]), receiver)
    else error(s"${manifest.name} has no field ${constValue[Name]}")

  transparent inline def structSet[S, Name <: String & Singleton](using
      manifest: StructManifest[S],
      m: Mirror.ProductOf[S]
  )(receiver: Rep[S], v: Rep[Field[Name, m.MirroredElemLabels, m.MirroredElemTypes]]) =
    inline if containsLabel[Name, m.MirroredElemLabels] then
      unsafeReflect(Op.StructSet(constValue[Name]), receiver, v)
    else error(s"${manifest.name} has no field ${constValue[Name]}")

  extension [S: StructManifest](t: Rep[S])
    inline def get[Name <: String & Singleton](using
        m: Mirror.ProductOf[S]
    ): Rep[Field[Name, m.MirroredElemLabels, m.MirroredElemTypes]] =
      structGet[S, Name](t)
    inline def set[Name <: String & Singleton](using
        m: Mirror.ProductOf[S]
    )(v: Rep[Field[Name, m.MirroredElemLabels, m.MirroredElemTypes]]) =
      structSet[S, Name](t, v)
}
