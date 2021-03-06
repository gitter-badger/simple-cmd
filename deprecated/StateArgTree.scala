//package com.github.cuzfrog.scmd.runtime
//
///*
// * StateArgTree is Not thread-safe. It should be only manipulated inside a Context.
// */
//
//private case class StateCmdNode(ref: CmdNode,
//                                params: Seq[StateParamNode],
//                                opts: Seq[StateOptNode],
//                                parent: Option[StateCmdNode],
//                                subCmdEntry: StateCmdEntryNode)
//
//private trait StateValueNode[+T <: StateValueNode[T]] extends StringUtils {
//  private[this] var value: Option[String] = None
//
//  def setValue(v: String): StateValueNode[T] = {
//    this.value = v
//    this
//  }
//
//  def getValue: Option[String] = value
//}
//
//private case class StateParamNode(ref: ParamNode[_]) extends StateValueNode[ParamNode[_]]
//private case class StateOptNode(ref: OptNode[_]) extends StateValueNode[OptNode[_]]
//private case class StateCmdEntryNode(ref: CmdEntryNode, children: Seq[StateCmdNode]) {
//  private[this] var enteredCmd: Option[StateCmdNode] = None
//
//  def enterCmd(cmd: String): Option[StateCmdNode] = {
//    enteredCmd = children.find(_.ref.entity.name == cmd)
//    enteredCmd
//  }
//}

//    /** Convert this immutable ArgTree to a stateful tree for further operation. */
//    def toStateTree: StateCmdNode = {
//      def recMakeStateful(cmdNode: CmdNode): StateCmdNode = {
//        require(cmdNode.parent.isEmpty, "Stateful conversion can only be done one top cmd node.")
//        val params = cmdNode.params.map(StateParamNode)
//        val opts = cmdNode.opts.map(StateOptNode)
//        val subCmdEntry =
//          StateCmdEntryNode(ref = cmdNode.subCmdEntry,
//            children = cmdNode.subCmdEntry.children.map(recMakeStateful))
//        StateCmdNode(ref = cmdNode, params = params, opts = opts,
//          parent = None, subCmdEntry = subCmdEntry)
//      }
//      recMakeStateful(a.toTopNode)
//    }