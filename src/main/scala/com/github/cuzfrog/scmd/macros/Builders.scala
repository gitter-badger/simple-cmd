package com.github.cuzfrog.scmd.macros

import scala.annotation.tailrec
import scala.collection.immutable
import scala.meta._

private object TreeBuilder {

  /**
    * Build a flat tree from arg definition by source code order.
    *
    * @param argDefs arg definition list from source code with original order.
    * @return A flat TermArgTree with at most one CmdEntryNode to first level sub-commands.
    */
  @inline
  def buildArgTreeByIdx(argDefs: immutable.Seq[TermArg]): TermArgTree = {
    @tailrec
    def recAdd(builder: IdxTermNodeBuilder, args: immutable.Seq[TermArg]): IdxTermNodeBuilder = {
      if (args.isEmpty) builder
      else recAdd(builder.add(args.head), args.tail)
    }

    argDefs.collectFirst { case cmd: TermCmd => cmd } match {
      case None =>
        val params = argDefs.collect { case param: TermParam => param }
        val opts = argDefs.collect { case opt: TermOpt => opt }
        TermArgTree(params, opts, TermCommandEntry.default)
      case Some(cmd1) =>
        val topLevelOpts = argDefs.filter(_.idx < cmd1.idx).collect {
          case opt: TermOpt => opt
          case param: TermParam => abort("Parameters cannot be defined before first command.")
        }
        val tail = argDefs.filter(_.idx > cmd1.idx)
        val builder = NodeBuilder.newIdxTermBuilder(cmd1)

        val commands = recAdd(builder, tail).seal
        TermArgTree(Nil, topLevelOpts, TermCommandEntry.defaultWithCmdNodes(commands))
    }
  }

  /**
    * Build tree from DSL.
    *
    * @return
    */
  def buildArgTreeByDSL(): TermArgTree = ???
}

private object NodeBuilder {
  def newIdxTermBuilder(cmd: TermCmd): IdxTermNodeBuilder = {
    new IdxTermNodeBuilder(cmd, None)
  }

  def newDslTermBuilder = ???
}

/** Not thread-safe */
private final class IdxTermNodeBuilder(cmd: TermCmd, lastSibling: Option[IdxTermNodeBuilder]) {

  private[this] var params: immutable.Seq[TermParam] = immutable.Seq.empty
  private[this] var opts: immutable.Seq[TermOpt] = immutable.Seq.empty

  def add(arg: TermArg): IdxTermNodeBuilder = arg match {
    case cmd: TermCmd => new IdxTermNodeBuilder(cmd, Option(this))
    case param: TermParam => this.params :+= param; this
    case opt: TermOpt => this.opts :+= opt; this
  }

  //non-defensive
  private def build: TermCmdNode = {
    TermCmdNode(cmd, params, opts, subCmdEntry = TermCommandEntry.default)
  }

  @inline
  def seal: immutable.Seq[TermCmdNode] = lastSibling match {
    case None => immutable.Seq(this.build)
    case Some(last) => last.seal :+ this.build
  }
}