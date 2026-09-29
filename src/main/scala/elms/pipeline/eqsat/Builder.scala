package elms.pipeline.eqsat

import scala.collection.mutable

import foresight.eqsat.{EClassCall, EClassRef}

import elms.core.given
import elms.core.{Type, Op, Name}
import elms.core.tree as ast
import elms.pipeline
import elms.util.{Counter, SourceContext}
import elms.util.Plumbing.*
import elms.util.collection.*
import elms.runtime.*

import Stmt.*

object Builder {
  case class Config(
      rules: Ruleset = Rules.default,
      cfg: EGraph.Config = EGraph.Config(),
      // Whether to drop a read whose value nothing wanted.
      //
      // On by default, and worth being able to turn off. A read is sometimes
      // written for something other than its value: a checker that reasons
      // about the residue sees one fewer access when the read goes, and a
      // bounds obligation goes with it.
      dropDeadReads: Boolean = true
  )
  enum Handle {
    case Global(name: Name)
    case Local(cls: EClassCall)
    // The EClassCall here is solely so downstream users can register nodes in the
    // EGraph; think of it as `v <- body`
    //
    // CR cwong: This representation is very scary; we're likely to drop effects
    // if we never actually write the body back into the CFG.
    case Region(v: Name, cls: EClassCall, body: Stmt)
  }
}

private class RegionBuilder(fresh: () => Name) {
  val body: mutable.ArrayBuffer[(Name, EClassCall, Stmt)] = mutable.ArrayBuffer()
  var tail: Option[EClassCall] = None

  def clear(): Unit = {
    tail = None
    body.clear()
  }

  def push(name: Name, cls: EClassCall, stmt: Stmt): Unit =
    body += ((name, cls, stmt))

  def ret(cls: EClassCall): Unit = { tail = Some(cls) }

  def extract(): Stmt = {
    val last = tail.getOrElse {
      throw LMSRuntimeException(
        "BUG: attempted to `RegionBuilder.finalize` with no `ret`"
      )
    }

    body.foldRight(Return(last)) { case ((name, _, stmt), acc) => Let(name, stmt, acc) }
  }
}

private class RegionStack(fresh: () => Name) {
  private val base: RegionBuilder = RegionBuilder(fresh)
  private val stack: mutable.Stack[RegionBuilder] = mutable.Stack()

  private def top: RegionBuilder = stack.peek.getOrElse(base)

  def openRegion(): Unit = stack.push(RegionBuilder(fresh))
  def closeRegion(): Stmt = stack.popSafe().map(_.extract()).getOrElse {
    Log.error("BUG: attempted to `closeRegion` with empty region stack")
    base.extract()
  }

  def push(name: Name, cls: EClassCall, stmt: Stmt): Unit = top.push(name, cls, stmt)

  def ret(cls: EClassCall): Unit = top.ret(cls)

  def isEmpty: Boolean = stack.isEmpty

  def extract(): Stmt = base.extract()
}

private class FunctionBuilder(
    name: Name,
    config: Builder.Config,
    predefs: Set[Name],
    fresh: () => Name
) {
  import Builder.Handle.*

  private val counter = Counter()
  private val graph = EGraph(config.rules, config.cfg)
  private val env = mutable.Map.from((predefs + name).map { name =>
    name -> graph.addNamedVar(name)
  })
  private val regions = RegionStack(fresh)

  // Reads already made and not yet invalidated, so a repeat of one is the name
  // the first one bound rather than a second load.
  //
  // Keyed on `EClassRef` and not `EClassCall`, for the reason `ScopeMap` is:
  // two calls can denote the same class, and a call held across a union denotes
  // nothing.
  private val reads = mutable.Map[(Op.Read, Seq[EClassRef]), EClassCall]()

  // The cell each read's binding carries, so elaboration can find it by the
  // name the binding introduced.
  private val weakByName = mutable.Map[Name, Weak]()

  private def invalidateReads(): Unit = reads.clear()

  def register(name: Name): EClassCall = {
    env(name) = graph.addNamedVar(name)
    env(name)
  }

  def ensureClass(name: Name): EClassCall = env.get(name).map(graph.canonical)
    .getOrElse {
      Log.warning(s"BUG: unregistered name $name was used before it was declared")
      register(name)
    }

  extension (handle: Builder.Handle)
    def unwrap: EClassCall = handle match {
      case Local(cls)              => cls
      case Global(name)            => ensureClass(name)
      case Region(name, cls, body) => {
        regions.push(name, cls, body)
        cls
      }
    }

    def asStmt: Stmt = handle match {
      case Local(cls)              => Return(cls)
      case Global(name)            => Return(ensureClass(name))
      case Region(name, cls, body) => body
    }

  private object Handle {
    def unapply(handle: Builder.Handle): Option[EClassCall] = Some(handle.unwrap)
  }

  def symbol(name: Name): EClassCall = graph.addNamedVar(name)

  def lambda(
      name: Name,
      cls: EClassCall,
      arg: Name,
      inty: Type,
      outty: Type,
      body: Builder.Handle
  ): Unit = regions.push(name, cls, Lambda(arg, inty, outty, body.asStmt))

  def reflect(op: Op, children: Seq[Builder.Handle]): Builder.Handle = op match {
    case pure: Op.Pure => reflectPure(pure, children.map(_.unwrap))
    case r: Op.Read    => reflectRead(r, children.map(_.unwrap))
    case w: Op.Write   => {
      invalidateReads()
      reflectEffect(w, children.map(_.unwrap))
    }
    case ctrl: Op.Control => reflectControl(ctrl, children)
  }

  private def reflectPure(op: Op.Pure, children: Seq[EClassCall]): Builder.Handle =
    Local(graph.addNode(op, children))

  private def reflectEffect(
      op: Op.Effectful,
      children: Seq[EClassCall]
  ): Builder.Handle = {
    val name = fresh()
    val cls = graph.addNamedVar(name)
    regions.push(name, cls, Effect(op, children))
    Local(cls)
  }

  private def reflectRead(op: Op.Read, children: Seq[EClassCall]): Builder.Handle = {
    val key = (op, children.map(graph.ref))

    Local(graph.canonical(reads.getOrElseUpdate(
      key, {
        val name = fresh()
        val cls = graph.addNamedVar(name)
        val weak = Weak()
        weakByName(name) = weak
        regions.push(name, cls, Read(op, children, weak))
        cls
      }
    )))
  }

  private def reflectControl(
      op: Op.Control,
      children: Seq[Builder.Handle]
  ): Builder.Handle = {
    val name = fresh()
    val cls = graph.addNamedVar(name)

    op match {
      case Op.IfThenElse => children match {
          case Seq(guard, thn, els) => regions
              .push(name, cls, If(guard.unwrap, thn.asStmt, els.asStmt))
          case _ => throw LMSRuntimeException("BUG: IfThenElse invalid children")
        }
      case Op.RangeForEach(x) => children match {
          case Seq(st, end, body) => regions
              .push(name, cls, RangeFor(x, st.unwrap, end.unwrap, body.asStmt))
          case _ => throw LMSRuntimeException("BUG: RangeForEach invalid children")
        }
      case Op.While => children match {
          case Seq(cond, body) => regions
              .push(name, cls, While(cond.asStmt, body.asStmt))
        }
      case Op.And => children match {
        case Seq(l, r) => {
          val falset = graph.addNode(Op.Const(false), Seq())
          regions.push(name, cls, If(l.unwrap, r.asStmt, Return(falset)))
        }
      }
      case Op.Or => children match {
        case Seq(l, r) => {
          val truet = graph.addNode(Op.Const(true), Seq())
          regions.push(name, cls, If(l.unwrap, Return(truet), r.asStmt))
        }
      }
    }
    Local(cls)
  }

  def ret(handle: Builder.Handle): Unit = regions.ret(handle.unwrap)

  // Both boundaries clear the memo, because the builder walks a region's body
  // once and in order. A write inside the body is reached after a read made
  // before the region, so at that read the memo still holds a value the write
  // is about to invalidate, and reusing it makes every iteration of a loop see
  // the original. There is no fixpoint here to discover that with, so the
  // boundary is where it gets paid for.
  def openRegion(): Unit = {
    invalidateReads()
    regions.openRegion()
  }

  def closeRegion(): (Name, EClassCall, Stmt) = {
    invalidateReads()
    val stmt = regions.closeRegion()
    val name = fresh()
    val cls = graph.addNamedVar(name)
    (name, cls, stmt)
  }

  def extract: ast.Term = {
    if !regions.isEmpty then {
      Log.warning("BUG: attempted to `extract` without closing all regions")
    }

    graph.saturate()
    val body = elab(regions.extract(), ScopeMap())
    if config.dropDeadReads then dropUndemanded(body) else body
  }

  // A read's class resolves to the name its statement bound, so any term the
  // elaboration produces that mentions that name is something wanting the read.
  private def recordDemand(t: ast.Term): Unit = t match {
    case ast.V(name)        => weakByName.get(name).foreach { _.demanded = true }
    case ast.E(_, children) => children.foreach(recordDemand)
    case ast.Let(_, e1, e2) => { recordDemand(e1); recordDemand(e2) }
    case ast.Function(_, _, _, body) => recordDemand(body)
  }

  // Drops the bindings whose weak cell was never set.
  //
  // No liveness analysis: the elaboration already recorded, per read, whether
  // anything resolved its class. All that is left is deleting the ones nothing
  // did, which is one walk and no per-binding scan of the body.
  private def dropUndemanded(t: ast.Term): ast.Term = t match {
    case ast.Let(x, e1, e2) => {
      val tail = dropUndemanded(e2)
      val bound = dropUndemanded(e1)

      if weakByName.get(x).exists(!_.demanded) then tail else ast.Let(x, bound, tail)
    }

    case ast.Function(arg, inty, outty, body) => ast
        .Function(arg, inty, outty, dropUndemanded(body))

    case ast.E(op, children) => ast.E(op, children.map(dropUndemanded))
    case v @ ast.V(_)        => v
  }

  // Keyed on `EClassRef` rather than `EClassCall`: two calls can denote the same
  // class, and a call stored before a union denotes nothing afterwards.
  class ScopeMap(
      parent: Option[ScopeMap] = None,
      vs: mutable.Map[EClassRef, Name] = mutable.Map()
  ) {
    def get(cls: EClassCall): Option[Name] = vs.get(graph.ref(cls))
      .orElse { parent.flatMap { _.get(cls) } }

    def update(cls: EClassCall, name: Name): Unit = { vs(graph.ref(cls)) = name }

    def enter: ScopeMap = ScopeMap(Some(this))
  }

  def elab(s: Stmt, cache: ScopeMap): ast.Term = {
    val (prefix, tail) = elabImpl(s, cache)
    prefix.foldRight(tail) { case ((x, e), acc) => ast.Let(x, e, acc) }
  }

  private type ElabOut = (Seq[(Name, ast.Term)], ast.Term)

  def elabCls(cls: EClassCall, cache: ScopeMap): ElabOut = cache.get(cls) match {
    case Some(v) => (Seq(), ast.V(v))
    case None    => {
      val name = fresh()
      val result = graph.extract(cls)
        .getOrElse { throw LMSRuntimeException(s"BUG: invalid EClassCall $cls") }
      recordDemand(result)
      cache(cls) = name
      (Seq((name, result)), ast.V(name))
    }
  }

  def elabImpl(s: Stmt, cache: ScopeMap): ElabOut = s match {
    case Return(cls)    => elabCls(cls, cache)
    case Let(x, e1, e2) => {
      val (prefix1, t1) = elabImpl(e1, cache)
      val (prefix2, t2) = elabImpl(e2, cache)
      ((prefix1 :+ (x, t1)) ++ prefix2, t2)
    }
    case Effect(op, children) => {
      children.map(elabCls(_, cache))
        .foldLeft(Vector.empty[(Name, (ast.Term))], Vector.empty[ast.Term]) {
          case ((prefixAcc, terms), (prefix, term)) =>
            (prefixAcc ++ prefix, terms :+ term)
        }.mapRight(ast.E(op, _))
    }
    case Read(op, children, _) => elabImpl(Effect(op, children), cache)
    case If(cond, thn, els)    => {
      val (prefix, c) = elabCls(cond, cache)
      val t = elab(thn, cache.enter)
      val e = elab(els, cache.enter)
      (prefix, ast.E(Op.IfThenElse, Seq(c, t, e)))
    }
    case Lambda(arg, inty, outty, body) =>
      (Seq(), ast.Function(arg, inty, outty, elab(body, cache.enter)))
    case RangeFor(x, st, end, body) => {
      val (prefix1, stt) = elabCls(st, cache)
      val (prefix2, endt) = elabCls(end, cache)
      (
        prefix1 ++ prefix2,
        ast.E(Op.RangeForEach(x), Seq(stt, endt, elab(body, cache.enter)))
      )
    }
    case While(cond, body) => {
      val guardt = elab(cond, cache.enter)
      val bodyt = elab(body, cache.enter)
      (Seq(), ast.E(Op.While, Seq(guardt, bodyt)))
    }
  }
}

class Builder(cfg: Builder.Config) extends pipeline.Builder {
  type Exp = Builder.Handle

  import Builder.Handle.*

  private enum FEntry derives CanEqual {
    case Stub
    case F(func: ast.Function)
  }
  import FEntry.*

  private val builtins = mutable.Set[Name]()
  private val functions = mutable.Map[Name, FEntry]()

  private var current: mutable.Stack[FunctionBuilder] = mutable.Stack()

  def predefs(): Set[Name] = builtins.toSet ++ functions.keySet

  def variable(name: Name): Exp = current.peek match {
    case None => {
      builtins.add(name)
      Global(name)
    }
    case Some(ctx) => Local(ctx.register(name))
  }

  private def ensureBuilder(msg: String): FunctionBuilder = current.peek
    .getOrElse { throw LMSRuntimeException(s"BUG: $msg") }

  private def topfun(name: Name, arg: Name, inty: Type, outty: Type): FunctionStub = {
    def fill(body: => Exp): Unit = {
      val builder = FunctionBuilder(name, cfg, predefs(), this.fresh)
      current.push(builder)
      val tail = body
      builder.ret(tail)
      val result = builder.extract
      current.pop()
      functions(name) = F(ast.Function(arg, inty, outty, result))
    }
    functions(name) = Stub
    current.foreach { _.register(name) }
    FunctionStub(Global(name), fill)
  }

  private def lambda(name: Name, arg: Name, inty: Type, outty: Type): FunctionStub = {
    val builder = ensureBuilder("attempted to define lambda outside function")
    val cls = builder.symbol(name)
    def fill(body: => Exp): Unit = builder
      .lambda(name, cls, arg, inty, outty, region(body))

    FunctionStub(Local(cls), fill)
  }

  def fun(name: Name, top: Boolean, arg: Name, inty: Type, outty: Type): FunctionStub =
    if top then topfun(name, arg, inty, outty) else lambda(name, arg, inty, outty)

  def reflect(op: Op, children: Seq[Exp]): Exp =
    ensureBuilder("attempted to `reflect` outside function").reflect(op, children)

  def region(f: => Exp): Exp = {
    val ctx = ensureBuilder("attempted to `region` outside function")
    ctx.openRegion()
    val tail = f
    ctx.ret(tail)
    val (name, cls, body) = ctx.closeRegion()
    Region(name, cls, body)
  }

  def extract(): ast.Program = {
    val funcs = functions.toSeq.filterMap {
      case (name, F(func)) => Some((name, func))
      case (_, Stub)       => {
        Log.warning(s"BUG: attempted to `extract` with function $name still stubbed")
        None
      }
    }
    ast.Program(funcs, staticData.toSeq)
  }
}
