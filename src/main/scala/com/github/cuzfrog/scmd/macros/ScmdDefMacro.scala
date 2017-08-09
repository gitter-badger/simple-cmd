package com.github.cuzfrog.scmd.macros


import com.github.cuzfrog.scmd.AppInfo
import com.github.cuzfrog.scmd.macros.Constants._
import com.github.cuzfrog.scmd.macros.argutils.ArgUtils

import scala.meta._

private class ScmdDefMacro extends ScmdMacro {


  /** Override this for testing. */
  protected val isTestMode: Boolean = true

  def expand(cls: Defn.Class): Stat = {
    val mods = cls.mods
    val name = cls.name
    val ctorMods = cls.ctor.mods
    val paramss = cls.ctor.paramss
    val stats = cls.templ.stats.getOrElse(Nil)

    /** For testing. */
    val privateMod = if (isTestMode) mod"private[scmd]" else mod"private[this]"

    /** args passed in from constructor. */
    val argsParam = paramss.flatten.headOption match {
      case Some(param"..$mods $name: Seq[String]") => Term.Name(name.value)
      case _ => abort(s"$name's first parameter must be of type Seq[String] to accept arguments.")
    }


    val appInfo = {
      val inferredName = if (name.value.endsWith("Def")) name.value.dropRight(3) else name.value
      TermAppInfo.collectAppInfo(stats, inferredName.toLowerCase)
    }
    implicit val _appInfo: AppInfo = appInfo.appInfo
    /**
      * A TermArg is macro time term of arg Node.
      *
      * This step collects arg defs from source code, checking syntax,
      * then turn them into Node terms.
      */
    val rawArgs = ArgUtils.collectRawArg(stats)
    val argDefs = TermArg.builtInArgs ++ rawArgs.map(TermArg.toTermArg)

    /**
      * ArgTree represents structure of user defined args.
      * A TermArgTree is a premature ArgTree consisting of Terms, Which contains the topological information.
      *
      * This step build an TermArgTree, then turn it into a single macro-reify-ready Term.
      *
      * This step needs a ScmdRuntime instance to execute.
      * ScmdRuntime serves as a runtime agent to instantiate and encapsulate all needed scmd classes.
      */
    val argTreeBuild = {
      val globalMutualLimitations = TermTree.collectArgGlobalLimitations(stats)
      val treeBuilder = TreeBuilder.builder
      TermTree.collectTreeDefDsl(stats) match {
        case Nil =>
          /* by the order of user-defined args in source code */
          treeBuilder.buildArgTreeByIdx(appInfo, argDefs, globalMutualLimitations).defnTerm
        case dslParams =>
          /* by tree def dsl */
          treeBuilder.buildArgTreeByDSL(
            appInfo, argDefs, dslParams, globalMutualLimitations).defnTerm
      }
    }

    /** Method expose to validation class for runtime manipulation. */
    val public_def_addValidation =
      q"""def addValidation[T](argName:String,func: T => Unit): this.type = {
            scmdRuntime.addValidation(argName,func)
            this
          }"""

    /**
      * Return a new defClass after done parsing.
      *
      * Built-in priors are run in the new defClass.
      * This does not conflict with client-defined control flow.
      * Because when a prior arg is matched, args(cmds) afterward are ignored,
      * they are not met to execute client code.
      */
    val public_def_parsed = {
      val termParamss = paramss.map(_.map(param => Term.Name(param.name.value)))
      q"""def parsed: $name = {
            val evaluatedDefClass = try{
              scmdRuntime.parse($argsParam)
              scmdRuntime.runBuiltInPriors()
              new ${Ctor.Ref.Name(name.value)}(...$termParamss){
                ..${ArgUtils.convertParsed(rawArgs)}
              }
            }catch{
              case e: ScmdException=> scmdRuntime.handleException(e)
            }
            //scmdRuntime.clean()
            evaluatedDefClass
          }"""
    }
    //todo: clean runtime after completion of parsing.

    val addMethods = List(
      q"""private[this] val scmdRuntime:ScmdRuntime = {
             val runtime = ScmdRuntime.create
             $argTreeBuild //execute scmdRuntime to build an argTree/appInfo
             runtime
          }""",
      q"implicit final def appInfo:AppInfo = scmdRuntime.getAppInfo",
      public_def_addValidation,
      q"def withValidation[T](vali: $name => T): this.type = {vali(this); this}",
      q"def runWithRoute(route: $name => ArgRoute): Boolean = {route(this.parsed).execute}",
      public_def_parsed
    )

    val testMethods = if (isTestMode) List(
      q"def usageString:String = scmdRuntime.usageString",
      q"def appInfoString:String = scmdRuntime.appInfoString",
      q"def argTreeString:String = scmdRuntime.argTreeString",
      q"def parsedSeqString:String = scmdRuntime.parsedSeqString",
      q"def parse():Unit = scmdRuntime.parse($argsParam)"
    ) else Nil

    val importTypes = List(
      importee"_",
      importee"ConversionOps => _",
      importee"PrettyStringBuildOps => _",
      importee"MergeOps => _",
      importee"MixOps => _"
    )

    //abort("dev...")
    q"""..$mods class $name ..$ctorMods (...$paramss){
          import $TERM_pkg_scmd._
          private val defApiImport = new ScmdDefApi{}
          import defApiImport._
          import $TERM_pkg_scmd.runtime._
          //import runtime.ScmdRuntime
          ..${ArgUtils.builtInPriorsStub}
          ..${ArgUtils.addExplicitType(rawArgs)}
          ..$addMethods
          ..$testMethods
        }"""
  }
}
