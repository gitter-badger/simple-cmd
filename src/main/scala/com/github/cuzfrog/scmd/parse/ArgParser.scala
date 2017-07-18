package com.github.cuzfrog.scmd.parse

import scala.reflect.ClassTag


private object ArgParser {
  def parse(argTree: ArgTree, args: Array[String]) = {
    val parsedTree = new BacktrackingParser(argTree, args).parsed
  }
}


private final class BacktrackingParser(argTree: ArgTree, args: Array[String]) {

  import BacktrackingParser._
  import scala.collection.mutable

  private[this] implicit val c: Context = new Context(argTree, args.map(categorize).toSeq)
  private[this] var paths: Anchor[CmdNode] = ???

  /**
    * Parse args against client defined argTree,
    * return a new tree only containing a path of parsed args.
    */
  def parsed: ArgTree = ???

  private def consume = {
    proceed match {
      case Some(ae)=>
      case None =>
    }
  }

  private def proceed: Option[AnchorEither] = {
    if (c.isComplete) None
    else if (c.mandatoryLeftDownstreamCnt > 0 && c.noArgLeft) {
      Some(ArgParseException("More args required", c))
    } else {
      c.nextCateArg.map(_.parsed)
    }
  }
}

private object BacktrackingParser {
  private val SingleOptExtractor = """-(\w{1}.*)""".r
  private val LongOptExtractor = """-((-[\w\d]+)+(=.*)?)""".r

  def categorize(arg: String): TypedArg[CateArg] = {
    val cateArg: CateArg = arg match {
      case SingleOptExtractor(sOpt) => SingleOpts(sOpt)
      case LongOptExtractor(lOpt) => LongOpt(lOpt)
      case paramOrCmd => ParamOrCmd(paramOrCmd)
    }
    TypedArg(cateArg, arg)
  }
}


