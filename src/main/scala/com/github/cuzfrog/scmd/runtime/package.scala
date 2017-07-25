package com.github.cuzfrog.scmd

import scala.reflect.ClassTag

package object runtime extends ArgTreeUtils {
  type AnchorEither = Either[ArgParseException, Seq[Anchor]]

  private[runtime] trait Parser[A, R] {
    def parse(a: A)(implicit c: Context): R
  }
  private[runtime] implicit class ParseOps[A, R](a: A)
                                                (implicit ev: Parser[A, R], context: Context) {
    private val parser = implicitly[Parser[A, R]]
    def parsed: R = parser.parse(a)
  }

  private[runtime] implicit class HSeqOps[N <: ValueNode](a: Seq[N]) {
    def collectWithType[T <: NodeTag[T] : ClassTag]: Seq[T] = {
      val classTag = implicitly[ClassTag[T]]
      a.collect {
        case node if node.tpe == classTag => node.asInstanceOf[T]
      }
    }
  }

  private[runtime] trait Countable[A] {
    def count(a: A): Int
  }
  private[runtime] implicit class CountOps[A: Countable](a: A) {
    private val countable = implicitly[Countable[A]]
    def count: Int = countable.count(a)
  }

  private[runtime] trait Collectible[A] {
    def collect[R: ClassTag](a: A): Seq[R]
  }
  private[runtime] implicit class CollectOps[A: Collectible](a: A) {
    private val collectible = implicitly[Collectible[A]]
    /** Given a type R, try to collect all reference to R in an A. */
    def collect[R: ClassTag]: Seq[R] = collectible.collect(a)
  }

  private[runtime] trait Convertible[A, R] {
    def convertTo(a: A): R
  }
  private[runtime] implicit class ConversionOps[A](a: A) {
    def convertTo[R](implicit ev: Convertible[A, R]): R = ev.convertTo(a)
  }

  private[runtime] trait Fillable[A, S] {
    def fillWith(a: A, stuff: S): A
  }
  private[runtime] implicit class FillOps[A, S](in: A)(implicit ev: Fillable[A, S]) {
    def fillWithStuff(stuff: S): A = ev.fillWith(in, stuff)
  }
}