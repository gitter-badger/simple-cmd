package com.github.cuzfrog.scmd

object ScmdRouteDSL {
  implicit final class RouteConditionsOps(in: RouteConditions) {
    def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
      new CmdRoute(in.cmd, in.conditions).run(innerF)
  }

  implicit final class CommandOps(cmd: Command) {
    def onConditions(cond1: RouteCondition,
                     condMore: RouteCondition*): RouteConditions =
      new RouteConditions(cmd, cond1 +: condMore)
    def run[R](innerF: => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
      new CmdRoute(cmd).run(innerF)
  }

  implicit final class SingleValueOps[T](a: SingleValue[T]) {
    def expect(compareF: Option[T] => Boolean): RouteCondition =
      new RouteCondition(compareF(a.value))
    def withValue[R](innerF: Option[T] => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
      new SingleValueRoute[T](a.value).withValue(innerF)
  }

  implicit final class SingleValueBooleanOps(a: SingleValue[Boolean]) {
    def expectTrue: RouteCondition = new RouteCondition(a.value.contains(true))
    def expectFalse: RouteCondition = new RouteCondition(a.value.contains(false))
  }

  implicit final class VariableValueOps[T](a: VariableValue[T]) {
    def expect(compareF: Seq[T] => Boolean): RouteCondition = new RouteCondition(compareF(a.value))
    def withValue[R](innerF: Seq[T] => R)(implicit ev: R <:< ArgRoute = null): ArgRoute =
      new VariableValueRoute[T](a.value).withValue(innerF)
  }

  implicit final class ScmdRouteOps(in: ArgRoute) {
    def ~(that: ArgRoute): ArgRoute = in match {
      case mergeRoute: MergeRoute => mergeRoute.copy(mergeRoute.seq :+ that)
      case other: ArgRoute => MergeRoute(Seq(other, that))
    }
  }

}
