package com.github.cuzfrog.scmd

import scala.meta._

final class ScmdDefTest extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods class $name { ..$stats }" =>
        macros.ScmdDefMacroTest.expand(name, stats)
      case _ =>
        abort("@ScmdDef must annotate an object.")
    }
  }
}