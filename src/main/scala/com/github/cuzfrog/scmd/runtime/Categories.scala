package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd.SingleValue
import com.github.cuzfrog.scmd.internal.SimpleLogging

import scala.annotation.tailrec
import scala.reflect.ClassTag

/*
 * The three categories of args provide low level parsing.
 */

private sealed trait CateArg {
  def arg: String
}
private object CateArg {
  implicit val parser: Parser[CateArg, AnchorEither] = new Parser[CateArg, AnchorEither] {
    override def parse(a: CateArg)(implicit context: Context): AnchorEither = a match {
      case s: SingleOpts => s.parsed
      case l: LongOpt => l.parsed
      case pm: ParamOrCmd => pm.parsed
    }
  }
}

/** Opt(s) with single letter. "-" has been stripped off. */
private case class SingleOpts(arg: String) extends CateArg
/** Opt with full name. One "-" of the "--" has been stripped off. */
private case class LongOpt(arg: String) extends CateArg
/** Param or Cmd with no prefix "-". */
private case class ParamOrCmd(arg: String) extends CateArg

/**
  * Option with single hyphen -
  * e.g. -p -P
  * -p=1234
  * folded -pwj = p+w+j  p,w,j must all be boolean.
  * multi-char -Pn  Pn is a single option abbreviation.
  */
private object SingleOpts extends CateUtils {
  private val EqualLitertal = """(\w+)\=(\w+)""".r
  private val ValueFolding = """(\w)([^\=]{1}.*)""".r

  implicit val parser: Parser[SingleOpts, AnchorEither] = new Parser[SingleOpts, AnchorEither] {
    override def parse(a: SingleOpts)(implicit c: Context): AnchorEither = {
      val arg = a.arg

      val matchOpt =
        c.getUpstreamLeftOpts.find(_.entity.abbr.exists(abbr => abbr == arg.take(abbr.length))) //match first letter
      matchOpt match {
        case Some(optNode1) =>
          trace(s"parse SingleOpts $arg matched -> ${optNode1.entity.name}[${optNode1.tpe}]")
          optNode1.tpe match {
            //found argDef for type Boolean
            case ClassTag.Boolean =>
              arg match {
                //arg=literal
                case EqualLitertal(argAbbr, bool) if optNode1.entity.abbr.contains(argAbbr) =>
                  parseBoolStr(bool) match {
                    case Some(b) => c.anchors(optNode1.copy(value = Seq(b)))
                    case None => ArgParseException(s"Unknown bool literal: $bool", c)
                  }
                //multichar single abbr
                case bool if optNode1.entity.abbr.contains(bool) =>
                  c.anchors(optNode1.copy(value = Seq(extractBooleanValue(optNode1))))
                //folded letters
                case bools if bools.matches("""\w+""") =>
                  val boolSet = bools.split("").toSet
                  /** Nodes with optDefs matched with args with type Boolean. */
                  val boolNodes = c.getUpstreamLeftOpts.collectWithType[OptNode[Boolean]]
                    .filter(n => n.entity.abbr.exists(boolSet.contains))
                  if (boolNodes.size < boolSet.size) {
                    val badArgs =
                      boolSet.filterNot(s => boolNodes.flatMap(_.entity.abbr).contains(s)).mkString
                    ArgParseException(s"Boolean options: -$badArgs not defined", c)
                  }
                  else if (boolSet.size < bools.length) {
                    ArgParseException(s"Duplicates in boolean options: -$bools", c)
                  }
                  else {
                    val optNodesWithValue = boolNodes.flatMap { n =>
                      c.anchors(n.copy(value = Seq(extractBooleanValue(n))))
                    }
                    optNodesWithValue
                  }

                case bad =>
                  ArgParseException(s"Boolean opts -$bad contain unsupported letter", c)
              }

            //found argDef for other types
            case otherTpe =>
              try {
                val value = arg match {
                  case EqualLitertal(_, v) => v
                  case ValueFolding(argAbbr, v) if optNode1.entity.abbr.contains(argAbbr) => v
                  case single if optNode1.entity.abbr.contains(single) => c.nextArg.getOrElse(
                    throw ArgParseException(
                      s"No value found for opt -$arg with type[${otherTpe.name}].", c))
                  case bad => throw ArgParseException(s"Malformed opt -$bad", c)
                }
                c.anchors(optNode1.copy(value = Seq(value)))
              } catch {
                case e: ArgParseException => e
              }
          }
        case None =>
          ArgParseException(s"Unknown opt -$arg", c)
      }
    }
  }
}

private object LongOpt extends CateUtils {
  private val EqualLiteral = """-([\-\w\d]+)(=.*)?""".r

  implicit val parser: Parser[LongOpt, AnchorEither] = new Parser[LongOpt, AnchorEither] {
    override def parse(a: LongOpt)(implicit c: Context): AnchorEither = {
      val arg = a.arg

      arg match {
        case EqualLiteral(argName, e_Value) =>
          val valueOpt = Option(e_Value).map(_.drop(1))
          val matchOpt = c.getUpstreamLeftOpts.find { n =>
            n.entity.name == argName || n.entity.hyphenName == argName
          }
          matchOpt match {
            case Some(optNode) =>
              trace(s"Parse LongOpt $arg -> matched [${optNode.tpe}]")
              optNode.tpe match {
                //arg def of type Boolean
                case ClassTag.Boolean =>
                  valueOpt match {
                    case Some(boolStr) => parseBoolStr(boolStr) match {
                      case Some(b) => c.anchors(optNode.copy(value = Seq(b)))
                      case None => ArgParseException(s"Unknown bool literal: -$arg", c)
                    }
                    case None =>
                      c.anchors(optNode.copy(value = Seq(extractBooleanValue(optNode))))
                  }

                //arg def of other type
                case otherTpe =>
                  val vOpt = valueOpt match {
                    case Some(v) => Some(v)
                    case None => c.nextArg
                  }
                  vOpt match {
                    case Some(v) => c.anchors(optNode.copy(value = Seq(v)))
                    case None =>
                      ArgParseException(
                        s"No value found for opt -$arg with type[${otherTpe.name}].", c)
                  }
              }

            case None =>
              trace(s"Parse LongOpt $arg -> not-matched.")
              ArgParseException(s"Unknown option: -$arg", c)
          }
        case bad => ArgParseException(s"Malformed option: $bad", c)
      }
    }
  }
}
private object ParamOrCmd extends CateUtils {
  implicit val parser: Parser[ParamOrCmd, AnchorEither] = new Parser[ParamOrCmd, AnchorEither] {
    override def parse(a: ParamOrCmd)
                      (implicit c: Context): AnchorEither = {
      val arg = a.arg

      c.nextParamNode match {
        //there's still params to match:
        case Some(paramNode) =>
          val anchorsWithValue: Seq[Anchor] =
            if (paramNode.isVariable) {
              //variable/multiple args:
              trace(s"Parse ParamOrCmd:${a.arg} -> param...")

              /** Pop args from context and create anchors along the way. */
              @tailrec
              def recFork(acc: Seq[Anchor], values: Seq[String]): Seq[Anchor] = {
                c.nextArgWithType[ParamOrCmd] match {
                  case Some(v) =>
                    val accValues = values :+ v
                    debug("ParamNode type before/after evaluation:" +
                      paramNode.tpe + "/" + paramNode.copy(value = accValues).tpe)
                    val newAnchor = c.anchors(paramNode.copy(value = accValues))
                    recFork(acc ++ newAnchor, accValues)
                  case None => acc
                }
              }

              val firstAnchor = c.anchors(paramNode.copy(value = Seq(arg)))
              recFork(firstAnchor, Seq(arg)) //current context state should point to last anchors.
            }

            else {
              //single arg:
              trace(s"Parse ParamOrCmd:${a.arg} -> param single")
              if (paramNode.isMandatory) c.anchors(paramNode.copy(value = Seq(arg)))
              else { //if the param is optional.
                val newAnchor = c.anchors(paramNode.copy(value = Seq(arg)))
                @tailrec
                def recFork(acc: Seq[Anchor]): Seq[Anchor] = {
                  c.nextParamNode match {
                    case Some(nextParamNode) if nextParamNode.isMandatory =>
                      acc :+ c.anchor(nextParamNode.copy(value = Seq(arg)))
                    case Some(nextParamNode) =>
                      recFork(acc :+ c.anchor(nextParamNode.copy(value = Seq(arg))))
                    case None => acc
                  }
                }
                recFork(newAnchor)
              }
            }

          val possibleCmdAnchor = if (!paramNode.entity.isMandatory) {
            this.consumeCmd(arg, c).right.toSeq.flatten
          } else Seq.empty

          anchorsWithValue ++ possibleCmdAnchor
        //there's no params before, a cmd should be matched:
        case None =>
          trace(s"Parse ParamOrCmd:${a.arg} -> cmd")
          this.consumeCmd(arg, c)
      }
    }

    /** Consume an arg as a cmd. */
    private def consumeCmd(arg: String, c: Context): AnchorEither = {
      c.nodeAdvance(arg) match {
        case Some(childCmdNode) => c.anchors(childCmdNode)
        case None => ArgParseException(s"Unknown cmd: $arg", c)
      }
    }
  }
}

private sealed trait CateUtils extends SimpleLogging {
  protected implicit
  def seqValue2Right[L](in: Seq[Anchor]): Either[L, Seq[Anchor]] = Right(in)

  /** Parse boolean string literal. */
  protected def parseBoolStr(in: String): Option[Boolean] = in.toLowerCase match {
    case "f" | "false" => Some(false)
    case "t" | "true" => Some(true)
    case _ => None
  }

  protected implicit class ClassTagOps[T](in: ClassTag[T]) {
    def name: String = in.runtimeClass.getSimpleName
  }

  protected implicit def boolean2string(b: Boolean): String = b.toString

  /** Trigger logic not of default boolean value. */
  protected def extractBooleanValue(n: OptNode[_]): Boolean = {
    if (n.tpe != ClassTag.Boolean)
      throw new AssertionError(s"Function extractBooleanValue can only used on boolean type," +
        s" node:${n.entity.name}")
    n.entity match {
      case s: SingleValue[Boolean@unchecked] => s.default.getOrElse(false).unary_!
      case _ =>
        throw new AssertionError(s"Boolean value cannot be variable." +
          s" Node:${n.entity.name}")
    }
  }
}