package lms.util.pretty

// Wadler/Leijen-style pretty-printing.

import java.io.Writer

enum Doc derives CanEqual {
  case Empty
  case Text(value: String)
  case Line
  case Cat(left: Doc, right: Doc)
  case Nest(indent: Int, doc: Doc)
  case Union(flat: Doc, broken: Doc)
}

object Doc {
  val empty: Doc = Empty

  def text(s: String): Doc = s.split('\n').map(Text.apply).fold(Empty)(Cat.apply)

  val line: Doc = Line

  val softline: Doc = group(line)
  val softbreak: Doc = group(linebreak)
  val linebreak: Doc = Union(Empty, Line)

  def nest(indent: Int)(doc: Doc): Doc = Nest(indent, doc)

  def group(doc: Doc): Doc = Union(flatten(doc), doc)

  def concat(docs: Iterable[Doc]): Doc = docs.foldLeft(empty)(_ <> _)

  def hsep(docs: Iterable[Doc]): Doc = docs.reduceOption(_ <+> _).getOrElse(empty)

  def vsep(docs: Iterable[Doc]): Doc = docs.reduceOption(_ </> _).getOrElse(empty)

  def sep(docs: Iterable[Doc]): Doc = group(vsep(docs))

  def punctuate(punctuation: Doc, docs: Iterable[Doc]): List[Doc] = {
    val xs = docs.toList

    xs match {
      case Nil => Nil
      case _   => xs.init.map(_ <> punctuation) :+ xs.last
    }
  }

  private def flatten(doc: Doc): Doc = {
    doc match {
      case Empty               => Empty
      case Text(value)         => Text(value)
      case Line                => Text(" ")
      case Cat(left, right)    => Cat(flatten(left), flatten(right))
      case Nest(indent, inner) => Nest(indent, flatten(inner))
      case Union(flat, _)      => flat
    }
  }

  private final case class Cmd(indent: Int, doc: Doc)

  def renderTo(doc: Doc, out: Writer, widthI: Int = 80): Unit = {
    val width = scala.math.max(10, widthI)

    def spaces(n: Int): Unit = out.write(' ' * n)

    def fits(remaining: Int, cmds: List[Cmd]): Boolean = remaining >= 0 &&
      (cmds match {
        case Nil                             => true
        case Cmd(_, Doc.Empty) :: rest       => fits(remaining, rest)
        case Cmd(_, Doc.Text(value)) :: rest => fits(remaining - value.length, rest)
        case Cmd(_, Doc.Line) :: _           => true
        case Cmd(indent, Doc.Cat(left, right)) :: rest =>
          fits(remaining, Cmd(indent, left) :: Cmd(indent, right) :: rest)
        case Cmd(indent, Doc.Nest(extra, inner)) :: rest =>
          fits(remaining, Cmd(indent + extra, inner) :: rest)
        case Cmd(indent, Doc.Union(flat, _)) :: rest =>
          fits(remaining, Cmd(indent, flat) :: rest)
      })

    def best(col: Int, cmds: List[Cmd]): Unit = cmds match {
      case Nil                             => {}
      case Cmd(_, Doc.Empty) :: rest       => best(col, rest)
      case Cmd(_, Doc.Text(value)) :: rest => {
        out.write(value)
        best(col + value.length, rest)
      }
      case Cmd(indent, Doc.Line) :: rest => {
        out.write('\n')
        spaces(indent)
        best(indent, rest)
      }

      case Cmd(indent, Doc.Cat(left, right)) :: rest =>
        best(indent, Cmd(indent, left) :: Cmd(indent, right) :: rest)
      case Cmd(indent, Doc.Nest(extra, inner)) :: rest =>
        best(indent, Cmd(indent + extra, inner) :: rest)

      case Cmd(indent, Doc.Union(flat, broken)) :: rest => {
        val flatCmds = Cmd(indent, flat) :: rest
        best(
          indent,
          if fits(width - col, flatCmds) then flatCmds else Cmd(indent, broken) :: rest
        )
      }
    }

    best(0, List(Cmd(0, doc)))
    out.flush()
  }

  def render(doc: Doc, width: Int = 80): String = {
    val w = new java.io.StringWriter()
    renderTo(doc, w, width)
    w.toString
  }
}

extension (left: Doc) {
  infix def <>(right: Doc): Doc = {
    (left, right) match {
      case (Doc.Empty, _) => right
      case (_, Doc.Empty) => left
      case _              => Doc.Cat(left, right)
    }
  }

  infix def <+>(right: Doc): Doc = left <> Doc.text(" ") <> right
  infix def </>(right: Doc): Doc = left <> Doc.softline <> right
  infix def <|/>(right: Doc): Doc = left <> Doc.softbreak <> right

  def renderTo(out: Writer, width: Int = 80): Unit = Doc.renderTo(left, out, width)
  def render(width: Int = 80): String = Doc.render(left, width)
}
