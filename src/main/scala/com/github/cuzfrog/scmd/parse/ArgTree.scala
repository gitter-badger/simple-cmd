package com.github.cuzfrog.scmd.parse

import com.github.cuzfrog.scmd.{CanFormPrettyString, Command, CommandEntry, OptionArg, Parameter}

import scala.reflect.ClassTag

private final case class ArgTree(topParams: Seq[ParamNode[_]],
                                 topOpts: Seq[OptNode[_]],
                                 cmdEntry: CmdEntryNode) {
  def toTopNode: CmdNode = CmdNode(
    parent = None,
    params = topParams,
    opts = topOpts,
    subCmdEntry = cmdEntry,
    entity = Command("AppName", None) //todo: replace AppName
  )
}

private sealed trait Node

private case class CmdNode(entity: Command,
                           params: Seq[ParamNode[_]],
                           opts: Seq[OptNode[_]],
                           parent: Option[CmdNode],
                           subCmdEntry: CmdEntryNode) extends Node

private case class CmdEntryNode(entity: CommandEntry,
                                children: Seq[CmdNode]) extends Node {
  lazy val mandatoryDownstreamCnt: Int = this.countMandatoryDownstream
}

private[parse] sealed trait NodeTag[+N <: NodeTag[N]]

private[parse] sealed trait ValueNode extends Node {
  def value: Seq[String]
  def tpe: ClassTag[_]
}

private case class ParamNode[+T](entity: Parameter[T],
                                 tpe: ClassTag[_],
                                 value: Seq[String])
  extends ValueNode with NodeTag[ParamNode[T]]

private case class OptNode[+T](entity: OptionArg[T],
                               tpe: ClassTag[_],
                               value: Seq[String])
  extends ValueNode with NodeTag[OptNode[T]] {

  //OptNode's equality depends on its entity's. Value is stripped off for parsing quick comparing.
  override def hashCode(): Int = entity.hashCode * 3 + 17
  override def equals(obj: scala.Any): Boolean = {
    if (!this.canEqual(obj)) return false
    obj.asInstanceOf[OptNode[_]].entity == this.entity
  }
  override def canEqual(that: Any): Boolean = that match {
    case _: OptNode[_] => true
    case _ => false
  }
  //todo: check if equals' overriding is correct.
}

//todo: find out is it able to new private class.

private object ArgTree {
  implicit val canFormPrettyString: CanFormPrettyString[ArgTree] = (a: ArgTree) => {
    val NEW_LINE = System.lineSeparator
    val cmdNode = a.toTopNode

    def recMkPrettyString(cmdNode: CmdNode, indent: String = ""): String = {
      val cmd = indent + cmdNode.entity.name
      val params =
        cmdNode.params.map(n => s"$indent+-param[${n.entity.name}] = ${n.value}")
      val opts =
        cmdNode.opts.map(n => s"$indent+-opt  [${n.entity.name}] = ${n.value}")
      val subCmds =
        cmdNode.subCmdEntry.children.map(n => recMkPrettyString(n, indent + "   "))
      val result: Seq[String] =
        Seq(cmd) ++ params ++ opts ++ Seq(s"$indent +-cmdEntry") ++ subCmds
      result.mkString(NEW_LINE)
    }

    recMkPrettyString(cmdNode)
  }
}