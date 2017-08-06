package com.github.cuzfrog.scmd

/** Dummy dsl. */
object ScmdTreeDefDSL {

  /** Entry to define g argument tree. */
  final def argTreeDef(subArg: Argument[_], moreSubArg: Argument[_]*): Unit = ()

  /** Define global argument mutual limitations. */
  final def argDependencyDef(group: MutualLimitation, moreGroup: MutualLimitation*): Unit = ()

  implicit final class CommandTreeDefOps(a: Command) {
    def apply(subArg: Argument[_], moreSubArg: Argument[_]*): Command = a

    /** Make this cmd optional. */
    def ? : Command = a
  }

  implicit final class ValueArgumentOps(a: ValueArgument[_]) {
    /** One of (mutually exclusive) */
    def |(that: ValueArgument[_]): ValueArgument[_] with MutuallyExclusive = {
      a.asInstanceOf[ValueArgument[_] with MutuallyExclusive]
    }

    /** Both of (mutually inclusive) */
    def &(that: ValueArgument[_]): ValueArgument[_] with MutuallyDependent = {
      a.asInstanceOf[ValueArgument[_] with MutuallyDependent]
    }
  }

  implicit final class MutuallyExclusiveOps(g: ValueArgument[_] with MutuallyExclusive) {
    /** One of (mutually exclusive) */
    def |(that: ValueArgument[_]): ValueArgument[_] with MutuallyExclusive = g
  }

  implicit final class MutuallyDependentOps(g: ValueArgument[_] with MutuallyDependent) {
    /** Both of (mutually inclusive) */
    def &(that: ValueArgument[_]): ValueArgument[_] with MutuallyDependent = g
  }

  // ------- Built in args --------
  /** Match: -help and --help */
  val help: PriorArg = Argument.BuiltInArgs.help
  /** Match: -version and --version */
  val version: PriorArg = Argument.BuiltInArgs.version

}
