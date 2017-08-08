import com.github.cuzfrog.scmd.{ArgRoute, PriorArg, ScmdApi, ScmdRouteDSL, ScmdSafeValueConverter, ScmdTreeDefDSL, ScmdValueConverter, ScmdValueImplicitConversion}
import com.github.cuzfrog.scmd.macros.MacroUtil

import scala.annotation.StaticAnnotation
import scala.meta._

/** Helper for client importing. */
package object Scmd extends ScmdApi {

  final val scmdTreeDefDSL: ScmdTreeDefDSL.type = ScmdTreeDefDSL
  final val scmdRouteDSL: ScmdRouteDSL.type = ScmdRouteDSL
  final val scmdValueImplicitConversion: ScmdValueImplicitConversion.type = ScmdValueImplicitConversion
  final val scmdValueConverter: ScmdValueConverter.type = ScmdValueConverter
  final val scmdSafeValueConverter: ScmdSafeValueConverter.type = ScmdSafeValueConverter

  // --------------------- Macro Annotations ----------------------
  final class ScmdDef extends StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
      MacroUtil('Def, defn)
    }
  }
  final class ScmdValid extends StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
      MacroUtil('Valid, defn)
    }
  }

  abstract class ScmdDefStub[D] {
    def withValidation[T](vali: D => T): this.type
    def runWithRoute(toRoute: D => ArgRoute): Boolean
    def parsed: D
    val help: PriorArg
    val version: PriorArg
  }
}

