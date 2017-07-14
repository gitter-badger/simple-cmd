package com.github.cuzfrog.scmd


sealed trait Argument[+A] {
  def name: String
  def description: Option[String]
}

case class Command(name: String, description: Option[String]) extends Argument[Nothing]

case class Parameter[T](name: String, description: Option[String] = None,
                        isMandatory: Boolean = Defaults.isMandatory) extends Argument[T]

case class OptionArg[T](name: String, description: Option[String] = None) extends Argument[T]
