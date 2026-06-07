package elms.codegen

import elms.core.{Type, Name, Op}
import elms.core.tree as ast
import elms.core.tree.View
import elms.util.IndentedWriter
import elms.util.pretty.Doc
import elms.runtime.Log

abstract class Backend(cfg: Config) {
  def emit(prog: ast.Program, out: java.io.PrintStream): Unit

  def render(prog: ast.Program): String = {
    val w = new java.io.ByteArrayOutputStream()
    emit(prog, new java.io.PrintStream(w))
    w.toString("utf-8")
  }

  protected def makeIndentedWriter(out: java.io.PrintStream): IndentedWriter =
    IndentedWriter(out, cfg.baseIndentLevel, cfg.indentKind)

  protected def renderType(ty: Type): String

  protected def v(name: Name): Doc
  protected def exp(op: Op, children: Seq[Doc], out: Type): Doc

  protected type Env = Map[Name, Type]

  protected def invalidTerm(msg: String): Doc = {
    Log.error(msg)
    invalidTermPlaceholder
  }

  protected def invalidTermPlaceholder: Doc

  protected def withView(t: ast.Term)(k: View => Doc): Doc = View.view(t)
      .fold { invalidTerm(s"Got invalid expression term: $t") }(k)

  protected def walk(env: Env)(t: ast.Term): Doc
}
