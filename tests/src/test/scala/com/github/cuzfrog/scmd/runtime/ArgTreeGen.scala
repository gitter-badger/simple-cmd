package com.github.cuzfrog.scmd.runtime

import com.github.cuzfrog.scmd._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._

import scala.reflect.ClassTag
//import org.scalacheck.ScalacheckShapeless._
import org.scalacheck._

private object ArgTreeGen {
  def arbName: Arbitrary[String] = Arbitrary(Gen.alphaStr suchThat (_.nonEmpty))
  //  def arbNames: Arbitrary[List[String]] =
  //    Arbitrary(Gen.containerOfN[List, String](10, arbName.arbitrary))
  def arbSymbol: Arbitrary[Symbol] = Arbitrary(arbName.arbitrary.map(Symbol(_)))
  def defaultValue: Arbitrary[(List[_], ClassTag[_])] = Arbitrary(Gen.oneOf(
    Gen.listOfN(5, Gen.alphaStr).map(l => (l, ClassTag(classOf[String]))),
    Gen.listOfN(5, arbitrary[Int]).map(l => (l, ClassTag.Int))
  ))

  // ================= Arguments ==================
  def arbCommand: Arbitrary[Command] = Arbitrary {
    for {
      name <- arbName.arbitrary
      dscr <- arbitrary[String]
    } yield Command(name, Option(dscr))
  }

  def arbCommandEntry: Arbitrary[CommandEntry] = Arbitrary {
    for (m <- arbitrary[Boolean]) yield CommandEntry(m)
  }

  def arbPriorArg: Arbitrary[PriorArg] = Arbitrary {
    for {
      name <- arbName.arbitrary
      dscr <- arbitrary[String]
      mtch <- arbitrary[Boolean]
    } yield PriorArg(name, Seq("-" + name), mtch, Option(dscr))
  }

  def arbArgValue[T]
  (implicit arbDefaultValue: Arbitrary[List[T]]): Arbitrary[ArgValue[T]] = Arbitrary {
    for {
      defaultValue <- arbDefaultValue.arbitrary
      argValue <- Gen.oneOf(
        ArgValue.single(defaultValue.headOption),
        ArgValue.variable(defaultValue)
      )
    } yield argValue
  }

  def arbParameter[T]
  (implicit arbDefaultValue: Arbitrary[List[T]])
  : Arbitrary[Parameter[T] with ArgValue[T]] = Arbitrary {
    import ScmdUtils._
    for {
      name <- arbName.arbitrary
      dscr <- arbitrary[String]
      argValue <- arbArgValue.arbitrary
      m <- arbitrary[Boolean]
    } yield mix[Parameter[T], ArgValue[T]](Parameter[T](name, Option(dscr), m), argValue)
  }

  def arbOptionArg[T]
  (implicit arbDefaultValue: Arbitrary[List[T]])
  : Arbitrary[OptionArg[T] with ArgValue[T]] = Arbitrary {
    import ScmdUtils._
    for {
      name <- arbName.arbitrary
      abbr <- Gen.oneOf(None, Option(name.take(1)), Option(name.take(1).toUpperCase))
      dscr <- arbitrary[String]
      argValue <- arbArgValue.arbitrary
      m <- arbitrary[Boolean]
    } yield mix[OptionArg[T], ArgValue[T]](OptionArg[T](name, abbr, Option(dscr), m), argValue)
  }

  def arbPropertyArg[T](implicit arbDefaultValue: Arbitrary[List[T]])
  : Arbitrary[PropertyArg[T] with VariableValue[(String, T)]] = Arbitrary {
    val defaultValues = Arbitrary(arbDefaultValue.arbitrary.map { vs =>
      vs.zipWithIndex.map {
        case (v, idx) => ("key" + idx, v)
      }
    })
    import ScmdUtils._
    for {
      name <- arbName.arbitrary
      dscr <- arbitrary[String]
      argValue <- arbArgValue(defaultValues).arbitrary suchThat (_.isVariable)
      m <- arbitrary[Boolean]
    } yield mix[PropertyArg[T], VariableValue[(String, T)]](
      PropertyArg[T](name, name.take(1).toUpperCase, Option(dscr)),
      argValue.asInstanceOf[VariableValue[(String, T)]]
    )
  }

  // ================= Nodes ==================
  def arbPriorNode: Arbitrary[PriorNode] = Arbitrary {
    for {
      e <- arbPriorArg.arbitrary
    } yield PriorNode(e, 'app)
  }

  def arbParamNode(arbDefaultValue: Arbitrary[(List[_], ClassTag[_])])
  : Arbitrary[ParamNode[_]] = Arbitrary {
    implicit val defaultValues: Arbitrary[List[_]] =
      Arbitrary(arbDefaultValue.arbitrary.map(_._1))
    for {
      e <- arbParameter.arbitrary
      tpe <- arbDefaultValue.arbitrary.map(_._2)
    } yield ParamNode(e, Nil, tpe, 'app)
  }

  def arbOptNode(arbDefaultValue: Arbitrary[(List[_], ClassTag[_])])
  : Arbitrary[OptNode[_]] = Arbitrary {
    implicit val defaultValues: Arbitrary[List[_]] =
      Arbitrary(arbDefaultValue.arbitrary.map(_._1))
    for {
      e <- arbOptionArg.arbitrary
      tpe <- arbDefaultValue.arbitrary.map(_._2)
    } yield OptNode(e, Nil, tpe, 'app)
  }

  def arbPropNode(arbDefaultValue: Arbitrary[(List[_], ClassTag[_])])
  : Arbitrary[PropNode[_]] = Arbitrary {
    implicit val defaultValues: Arbitrary[List[_]] =
      Arbitrary(arbDefaultValue.arbitrary.map(_._1))
    for {
      e <- arbPropertyArg.arbitrary
      tpe <- arbDefaultValue.arbitrary.map(_._2)
    } yield PropNode(e, Nil, tpe)
  }

  def arbParamNodes: Arbitrary[List[ParamNode[_]]] = Arbitrary {
    for {
      n <- arbitrary[Int] suchThat (_ < 10)
      paramNode <- arbParamNode(defaultValue).arbitrary
      seq <- Gen.listOfN(n, paramNode)
    } yield seq
  }

  def genArgTree = {
    //implicit val option: Arbitrary[Option[String]] = Arbitrary.arbOption[String]
  }


}

import org.junit._
import org.junit.Assert._

class ArgTreeGenTest extends ScalacheckIntegration {

  import ArgTreeGen._

  @Test
  def test1(): Unit = {

    val result = forAll { e: PriorNode =>
      e.entity.name.length < 50
    }

    assertTrue("prop not holds", result)
  }

}