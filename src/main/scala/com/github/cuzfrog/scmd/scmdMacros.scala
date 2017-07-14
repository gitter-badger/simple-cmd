package com.github.cuzfrog.scmd

import scala.collection.immutable
import scala.meta._
import scala.reflect.ClassTag

class ScmdDef extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods object $name { ..$stats }" =>
        ScmdDefMacroImpl.expand(name, stats)
      case _ =>
        abort("@ScmdDef must annotate an object.")
    }
  }
}

private object ScmdDefMacroImpl {
  private val TERM_NAME = Term.Name("name")
  private val TERM_DESCRIPTION = Term.Name("description")
  private val TERM_IS_MANDATORY = Term.Name("isMandatory")
  private val TYPE_NOTHING = Type.Name("Nothing")

  def expand(name: Term.Name, stats: immutable.Seq[Stat]): Defn.Object = {

    val argDefs = collectRawArg(stats)


    //reifiedArgs.foreach(t => println(t.structure))

    //    argDefs.foreach(println)
    //    println(ArgBuilder.buildArgGraphByIdx(argDefs))

    val addMethods = Seq(
      //q"val argDefs = _root_.scala.collection.Seq(..$argDefs)",
      q"def parse(args: Array[String]): Unit = { args.foreach(println) }"
    )
    val moreStats = stats ++ addMethods

    abort("dev...")

    q"object $name { ..$moreStats }"
  }


  private def collectRawArg(stats: Seq[Stat]): Seq[RawArg] = {
    stats.zipWithIndex collect {
      case (q"val $cmd: $_ = cmdDef($dscr)", idx) =>
        val dscrContent = dscr match {
          case Term.Arg.Named(_, Lit.String(c)) => c
          case Lit.String(c) => c
        }
        RawArg(Command(cmd.syntax, dscrContent), idx, TYPE_NOTHING)

      case (q"val $para = paraDef[$tpe](..$params)", idx) =>
        val dscrCtnt = params.collect {
          case Term.Arg.Named(TERM_DESCRIPTION, Lit.String(c)) => c
          case Lit.String(c) => c
        }.headOption
        val isMandatory = params.collect {
          case Term.Arg.Named(TERM_IS_MANDATORY, Lit.Boolean(b)) => b
          case Lit.Boolean(b) => b
        }.headOption.getOrElse(Defaults.isMandatory)
        RawArg(
          Parameter(name = para.syntax, description = dscrCtnt, isMandatory = isMandatory)
          , idx, tpe
        )
    }
  }

  private def raw2termArg(rawArg: RawArg): TermArg = {
    val name = Term.Name(rawArg.arg.name)
    val desciption = rawArg.arg.description match {
      case Some(dscr) => q"Option(${Lit.String(dscr)})"
      case None => q"None"
    }
    val (term, argTpe) = rawArg.arg match {
      case _: Command =>
        (q"Command($TERM_NAME = $name,$TERM_DESCRIPTION = $desciption)", ArgType.Cmd)
    }
    TermArg(term, argTpe, rawArg.idx, rawArg.tpe)
  }
  // ============== Helpers ================
  implicit def string2option(s: String): Option[String] = if (s == "") None else Option(s)
}

